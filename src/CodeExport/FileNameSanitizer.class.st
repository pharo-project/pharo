"
I sanitize strings to be valid, cross-platform filenames across Windows, macOS, and Linux. I replace OS-forbidden characters, control characters, and trim trailing dots and whitespace. I also escape reserved device names on Windows (such as CON, PRN, AUX, NUL, COM1-9, LPT1-9).
"
Class {
	#name : 'FileNameSanitizer',
	#superclass : 'Object',
	#category : 'CodeExport-Utilities',
	#package : 'CodeExport',
	#tag : 'Utilities'
}

{ #category : 'sanitizing' }
FileNameSanitizer class >> sanitize: aString [
	| forbidden reservedNames sanitized dotIndex base ext |
	forbidden := { $\. $/. $:. $*. $?. $". $<. $>. $| }.
	reservedNames := #( 'CON' 'PRN' 'AUX' 'NUL'
		'COM1' 'COM2' 'COM3' 'COM4' 'COM5' 'COM6' 'COM7' 'COM8' 'COM9'
		'LPT1' 'LPT2' 'LPT3' 'LPT4' 'LPT5' 'LPT6' 'LPT7' 'LPT8' 'LPT9' ).

	sanitized := aString collect: [ :each |
		(each asciiValue < 32 or: [ each asciiValue = 127 or: [ forbidden includes: each ] ])
			ifTrue: [ $_ ]
			ifFalse: [ each ] ].

	sanitized := sanitized trimBoth.
	sanitized ifEmpty: [ ^ 'unnamed' ].

	dotIndex := sanitized findLast: [ :each | each = $. ].
	(dotIndex > 1 and: [ dotIndex < sanitized size ])
		ifTrue: [
			base := sanitized copyFrom: 1 to: dotIndex - 1.
			ext := sanitized copyFrom: dotIndex to: sanitized size ]
		ifFalse: [
			base := sanitized.
			ext := '' ].

	base := base trimRight: [ :each | each = $. or: [ each isSeparator ] ].
	base ifEmpty: [ base := 'unnamed' ].

	(reservedNames includes: base asUppercase)
		ifTrue: [ base := '_' , base ].

	^ base , ext
]
