"
typedef struct tagOFN { 
  DWORD         lStructSize; 
  HWND          hwndOwner; 
  HINSTANCE     hInstance; 
  LPCTSTR       lpstrFilter; 
  LPTSTR        lpstrCustomFilter; 
  DWORD         nMaxCustFilter; 
  DWORD         nFilterIndex; 
  LPTSTR        lpstrFile; 
  DWORD         nMaxFile; 
  LPTSTR        lpstrFileTitle; 
  DWORD         nMaxFileTitle; 
  LPCTSTR       lpstrInitialDir; 
  LPCTSTR       lpstrTitle; 
  DWORD         Flags; 
  WORD          nFileOffset; 
  WORD          nFileExtension; 
  LPCTSTR       lpstrDefExt; 
  LPARAM        lCustData; 
  LPOFNHOOKPROC lpfnHook; 
  LPCTSTR       lpTemplateName; 
#if (_WIN32_WINNT >= 0x0500)
  void *        pvReserved;
  DWORD         dwReserved;
  DWORD         FlagsEx;
#endif // (_WIN32_WINNT >= 0x0500)
} OPENFILENAME, *LPOPENFILENAME;

"
Class {
	#name : #OPENFILENAME,
	#superclass : #Alien,
	#type : #bytes,
	#category : 'Alien-Win32'
}

{ #category : #'instance creation' }
OPENFILENAME class >> dataSize [
	"sizeof(OPENFILENAME) in Windows 5 (76 in Windows 4)"
	^88
	
"
typedef struct tagOFN { 
  DWORD         lStructSize; 
  HWND          hwndOwner; 
  HINSTANCE     hInstance; 
  LPCTSTR       lpstrFilter; 
  LPTSTR        lpstrCustomFilter; 
  DWORD         nMaxCustFilter; 
  DWORD         nFilterIndex; 
  LPTSTR        lpstrFile; 
  DWORD         nMaxFile; 
  LPTSTR        lpstrFileTitle; 
  DWORD         nMaxFileTitle; 
  LPCTSTR       lpstrInitialDir; 
  LPCTSTR       lpstrTitle; 
  DWORD         Flags; 
  WORD          nFileOffset; 
  WORD          nFileExtension; 
  LPCTSTR       lpstrDefExt; 
  LPARAM        lCustData; 
  LPOFNHOOKPROC lpfnHook; 
  LPCTSTR       lpTemplateName; 
#if (_WIN32_WINNT >= 0x0500)
  void *        pvReserved;
  DWORD         dwReserved;
  DWORD         FlagsEx;
#endif // (_WIN32_WINNT >= 0x0500)
} OPENFILENAME, *LPOPENFILENAME;
"
]

{ #category : #constants }
OPENFILENAME >> MAXPATH [
	^260
]

{ #category : #accessing }
OPENFILENAME >> fileName [
	^(Alien forPointer: (self unsignedLongAt: self lpstrFileOffset)) strcpyUTF8
]

{ #category : #'memory management' }
OPENFILENAME >> free [
	self class primFree: (self unsignedLongAt:  self lpstrFileOffset).
	super free
]

{ #category : #'instance initalization' }
OPENFILENAME >> initialize [
	self
		unsignedLongAt: self lStructSizeOffset put: self class dataSize;
		unsignedLongAt: self lpstrFileOffset put: (self class Ccalloc: self MAXPATH);
		unsignedLongAt: self nMaxFileOffset put: self MAXPATH
]

{ #category : #'field offsets' }
OPENFILENAME >> lStructSizeOffset [
	^1 "0"
]

{ #category : #'field offsets' }
OPENFILENAME >> lpstrFileOffset [
	^29 "28"
]

{ #category : #'field offsets' }
OPENFILENAME >> nMaxFileOffset [
	^33 "32"
]
