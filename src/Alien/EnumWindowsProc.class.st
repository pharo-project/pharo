"
BOOL CALLBACK EnumWindowsProc(HWND hwnd, LPARAM lParam);

"
Class {
	#name : #EnumWindowsProc,
	#superclass : #Alien,
	#type : #bytes,
	#category : #'Alien-Win32'
}

{ #category : #'instance creation' }
EnumWindowsProc class >> dataSize [
	^8
]

{ #category : #accessing }
EnumWindowsProc >> hwnd [
	"BOOL CALLBACK EnumWindowsProc(HWND hwnd, LPARAM lParam);"
	^self unsignedLongAt: 1
]

{ #category : #accessing }
EnumWindowsProc >> lParam [
	"BOOL CALLBACK EnumWindowsProc(HWND hwnd, LPARAM lParam);"
	^self unsignedLongAt: 5
]
