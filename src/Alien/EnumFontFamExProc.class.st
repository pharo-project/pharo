"
int CALLBACK EnumFontFamExProc(
  ENUMLOGFONTEX *lpelfe,    // logical-font data
  NEWTEXTMETRICEX *lpntme,  // physical-font data
  DWORD FontType,           // type of font
  LPARAM lParam             // application-defined data
);

"
Class {
	#name : #EnumFontFamExProc,
	#superclass : #Alien,
	#type : #bytes,
	#category : #'Alien-Win32'
}

{ #category : #'instance creation' }
EnumFontFamExProc class >> dataSize [
	^16
]

{ #category : #accessing }
EnumFontFamExProc >> FontType [
	"DWORD FontType,           // type of font"
	^self unsignedLongAt: 9
]

{ #category : #accessing }
EnumFontFamExProc >> lParam [
	"LPARAM lParam             // application-defined data"
	^self unsignedLongAt: 13
]

{ #category : #accessing }
EnumFontFamExProc >> logicalFontData [
	"Convenience for ENUMLOGFONTEX *lpelfe,    // logical-font data"
	^ENUMLOGFONTEXA atAddress: (self unsignedLongAt: 1)
]

{ #category : #accessing }
EnumFontFamExProc >> lpelfe [
	"ENUMLOGFONTEX *lpelfe,    // logical-font data"
	^self unsignedLongAt: 1
]

{ #category : #accessing }
EnumFontFamExProc >> lpntme [
	"NEWTEXTMETRICEX *lpntme,  // physical-font data"
	^self unsignedLongAt: 5
]
