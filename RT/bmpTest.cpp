// bmptest.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include <iostream>
#include <fstream>
#include <cmath>
#include <string>
#include <vector>

using namespace std;

const int WIDTH = 512;// (720 / 9) * 16;
const int HEIGHT = 512;// 720;

struct vec3 {
	float x, y, z;
};
struct ivec3 {
	int x, y, z;
};
struct complex {
	float a, bi;
};

complex operator*(const complex& a, const complex& b) {
	return complex({ a.a * b.a + a.bi * b.bi, a.a * b.bi + a.bi * b.a });
}
complex operator+(const complex& a, const complex& b) {
	return complex({ a.a + b.a, a.bi + b.bi });
}
ostream& operator<<(ostream& os, const complex& c) {
	os << c.a;
	os << (c.bi > 0) ? "+" : "-";
	os << c.bi << "i";
	return os;
}

vec3 img[WIDTH][HEIGHT];
ivec3 img_redu[WIDTH][HEIGHT];

int reduce(float val) {
	return int(val * 255.0);
}

//val = (1.0-fac)*oldmin+fac*oldmax
//val-oldmin = fac*(oldmax-oldmin)
//(val-oldmin)/(oldmax-oldmin)=fac
float remap(float oldmin, float oldmax, float newmin, float newmax, float val) {
	float fac = (val - oldmin) / (oldmax - oldmin);
	return (1.0 - fac) * newmin + fac * newmax;
}

void makeSimpleGrad() {
	for (int i = 0; i < WIDTH; i++) {
		for (int j = 0; j < HEIGHT; j++) {
			float rei = remap(0, WIDTH - 1, 0.0, 1.0, i);
			float rej = remap(0, HEIGHT - 1, 0.0, 1.0, j);
			float k = remap(-2, 2, 0.0, 1.0, sin(rei) + cos(rej));
			rei = fmod(rei * 10.0, 1.0);
			rej = fmod(rej * rei * 5.0 + 3.0, 1.0);
			img[i][j] = vec3({ rei, rej, k });
		}
	}
}

//Modified stackoverflow code
void drawbmp(const char* filename) {

	unsigned int headers[13];
	FILE* outfile;
	int extrabytes;
	int paddedsize;
	int x; int y; int n;
	int red, green, blue;

	extrabytes = 4 - ((WIDTH * 3) % 4);                 // How many bytes of padding to add to each
														// horizontal line - the size of which must
														// be a multiple of 4 bytes.
	if (extrabytes == 4)
		extrabytes = 0;

	paddedsize = ((WIDTH * 3) + extrabytes) * HEIGHT;

	// Headers...
	// Note that the "BM" identifier in bytes 0 and 1 is NOT included in these "headers".

	headers[0] = paddedsize + 54;      // bfSize (whole file size)
	headers[1] = 0;                    // bfReserved (both)
	headers[2] = 54;                   // bfOffbits
	headers[3] = 40;                   // biSize
	headers[4] = WIDTH;  // biWidth
	headers[5] = HEIGHT; // biHeight

	// Would have biPlanes and biBitCount in position 6, but they're shorts.
	// It's easier to write them out separately (see below) than pretend
	// they're a single int, especially with endian issues...

	headers[7] = 0;                    // biCompression
	headers[8] = paddedsize;           // biSizeImage
	headers[9] = 0;                    // biXPelsPerMeter
	headers[10] = 0;                    // biYPelsPerMeter
	headers[11] = 0;                    // biClrUsed
	headers[12] = 0;                    // biClrImportant

	fopen_s(&outfile, filename, "wb");
	if (outfile == NULL) return;
	//
	// Headers begin...
	// When printing ints and shorts, we write out 1 character at a time to avoid endian issues.
	//

	fprintf(outfile, "BM");

	for (n = 0; n <= 5; n++)
	{
		fprintf(outfile, "%c", headers[n] & 0x000000FF);
		fprintf(outfile, "%c", (headers[n] & 0x0000FF00) >> 8);
		fprintf(outfile, "%c", (headers[n] & 0x00FF0000) >> 16);
		fprintf(outfile, "%c", (headers[n] & (unsigned int)0xFF000000) >> 24);
	}

	// These next 4 characters are for the biPlanes and biBitCount fields.

	fprintf(outfile, "%c", 1);
	fprintf(outfile, "%c", 0);
	fprintf(outfile, "%c", 24);
	fprintf(outfile, "%c", 0);

	for (n = 7; n <= 12; n++)
	{
		fprintf(outfile, "%c", headers[n] & 0x000000FF);
		fprintf(outfile, "%c", (headers[n] & 0x0000FF00) >> 8);
		fprintf(outfile, "%c", (headers[n] & 0x00FF0000) >> 16);
		fprintf(outfile, "%c", (headers[n] & (unsigned int)0xFF000000) >> 24);
	}

	//
	// Headers done, now write the data...
	//

	for (y = HEIGHT - 1; y >= 0; y--)     // BMP image format is written from bottom to top...
	{
		for (x = 0; x <= WIDTH - 1; x++)
		{

			red = reduce(img[x][y].x);
			green = reduce(img[x][y].y);
			blue = reduce(img[x][y].z);

			if (red > 255) red = 255; if (red < 0) red = 0;
			if (green > 255) green = 255; if (green < 0) green = 0;
			if (blue > 255) blue = 255; if (blue < 0) blue = 0;

			// Also, it's written in (b,g,r) format...

			fprintf(outfile, "%c", blue);
			fprintf(outfile, "%c", green);
			fprintf(outfile, "%c", red);
		}
		if (extrabytes)      // See above - BMP lines must be of lengths divisible by 4.
		{
			for (n = 1; n <= extrabytes; n++)
			{
				fprintf(outfile, "%c", 0);
			}
		}
	}

	fclose(outfile);
	return;
}

string whitelist = "0123456789.";
bool whitelisted_char(char c) {
	return whitelist.find(c) != string::npos;
}

inline bool beginsWith(string beg, string str, int offset) {
	if (str.size() < beg.size()) return false;
	for (int i = 0; i < beg.size(); i++) {
		if (beg[i] != str[i + offset]) return false;
	}
	return true;
}

string filter_text(string text) {
	clog << "Filtering input file text of size "<<text.length()<<"...\n";
	string res;
	int mark = text.length() / 10, marklen=mark;
	for (int i = 0; i < text.length(); i++) {
		if (i == mark) {
			cout << mark / marklen << "0%...";
			mark += marklen;
		}
		if (whitelisted_char(text[i])) {
			res.push_back(text[i]);
		} else {
			if (!empty(res)) {
				if (res.back() != ' ') res.push_back(' ');
			}
		}
	}
	clog << "\n";
	return res;
}

template<typename t>
void printVec(vector<t> vec) {
	for (int i = 0; i < vec.size(); i++) {
		cout << vec[i] << " ";
	}
	cout << "\nSize: "<<vec.size() << "\n";
}
vector<string> words(string unsplit) {
	clog << "Splitting text...\n";
	string curr;
	vector < string > res;
	for (int i = 0; i < unsplit.length(); i++) {
		if (unsplit[i] == ' ') {
			if(!curr.empty())
				res.push_back(curr);
			curr.clear();
		} else {
			curr.push_back(unsplit[i]);
		}
	}
	if (!curr.empty()) res.push_back(curr);
	return res;
}

vector<float> toFloats(vector<string> strings) {
	clog << "Converting to floats...\n";
	vector<float> res;
	for (int i = 0; i < strings.size(); i++) {
		res.push_back(stof(strings[i]));
	}
	return res;
}

void writeToImgArr(vector<float> floats) {
	clog << "Writing internally " << floats.size() <<" floats (" << floats.size()/3 << " pixels)...\n";
	if (floats.size() % 3 != 0 || (floats.size()/WIDTH != HEIGHT) || (floats.size() % WIDTH != 0)) clog << "Incorrect amount of numbers!\n";
	vec3 inVec;
	for (int i = 0; i < floats.size(); i++) {
		switch (i % 3) {
		case 0:
			inVec.x = floats[i]; 
			//clog << "(r: " << inVec.x;
			break;
		case 1:
			inVec.y = floats[i]; 
			//clog << ", g:" << inVec.y;
			break;
		case 2:
			inVec.z = floats[i]; 
			//clog << ", b: " << inVec.z << ") ";
			break;
		}
		if (i%3 == 2) {
			int floori = i / 3;
			img[floori % WIDTH][floori / WIDTH] = inVec;
		}
	}
}

void readFile(const char* filepath = "C:\\Users\\Vikto\\Documents\\raytraceTest\\test2.txt") {
	ifstream ifile(filepath);
	char in;
	bool unicode_extra = false;
	string readRes;
	clog << "Reading from file...\n";
	while (ifile >> in) {
		readRes.push_back(in);
	}
	//cout << readRes;
	//cout << filter_text(readRes);
	writeToImgArr(toFloats(words(filter_text(readRes))));
}

int main()
{
	//clog << "Generating image...\n";
	//makeSimpleGrad();
	clog << "Reading image...\n";
	readFile();
	clog << "Done. Writing file...\n";
	drawbmp("testbmp.bmp");
	clog << "Done. Exiting.\n";
}

// Run program: Ctrl + F5 or Debug > Start Without Debugging menu
// Debug program: F5 or Debug > Start Debugging menu

// Tips for Getting Started: 
//   1. Use the Solution Explorer window to add/manage files
//   2. Use the Team Explorer window to connect to source control
//   3. Use the Output window to see build output and other messages
//   4. Use the Error List window to view errors
//   5. Go to Project > Add New Item to create new code files, or Project > Add Existing Item to add existing code files to the project
//   6. In the future, to open this project again, go to File > Open > Project and select the .sln file