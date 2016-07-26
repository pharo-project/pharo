accessing
hwnd
	"BOOL CALLBACK EnumWindowsProc(HWND hwnd, LPARAM lParam);"
	^self unsignedLongAt: 1