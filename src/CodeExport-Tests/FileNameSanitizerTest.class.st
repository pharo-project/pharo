Class {
	#name : 'FileNameSanitizerTest',
	#superclass : 'TestCase',
	#category : 'CodeExport-Tests-Utilities',
	#package : 'CodeExport-Tests',
	#tag : 'Utilities'
}

{ #category : 'tests' }
FileNameSanitizerTest >> testSanitizeCleanName [
	self assert: (FileNameSanitizer sanitize: 'MyClass') equals: 'MyClass'.
	self assert: (FileNameSanitizer sanitize: 'Point-x') equals: 'Point-x'
]

{ #category : 'tests' }
FileNameSanitizerTest >> testSanitizeControlCharacters [
	| raw |
	raw := String with: $a with: Character null with: Character cr with: Character lf with: $b.
	self assert: (FileNameSanitizer sanitize: raw) equals: 'a___b'
]

{ #category : 'tests' }
FileNameSanitizerTest >> testSanitizeEmptyAndWhitespace [
	self assert: (FileNameSanitizer sanitize: '') equals: 'unnamed'.
	self assert: (FileNameSanitizer sanitize: '   ') equals: 'unnamed'.
	self assert: (FileNameSanitizer sanitize: '...') equals: 'unnamed'
]

{ #category : 'tests' }
FileNameSanitizerTest >> testSanitizeForbiddenCharacters [
	| forbidden raw sanitized |
	forbidden := { $\. $/. $:. $*. $?. $". $<. $>. $| }.
	raw := String streamContents: [ :s |
		s nextPutAll: 'foo'.
		forbidden do: [ :c | s nextPut: c ].
		s nextPutAll: 'bar' ].
	sanitized := FileNameSanitizer sanitize: raw.
	forbidden do: [ :c | self deny: (sanitized includes: c) ].
	self assert: sanitized equals: 'foo_________bar'
]

{ #category : 'tests' }
FileNameSanitizerTest >> testSanitizeMethodSelector [
	self assert: (FileNameSanitizer sanitize: 'Point-at:put:') equals: 'Point-at_put_'.
	self assert: (FileNameSanitizer sanitize: 'Point-/') equals: 'Point-_'.
	self assert: (FileNameSanitizer sanitize: 'Point-//') equals: 'Point-__'
]

{ #category : 'tests' }
FileNameSanitizerTest >> testSanitizeTrailingDotsAndSpaces [
	self assert: (FileNameSanitizer sanitize: 'test.') equals: 'test'.
	self assert: (FileNameSanitizer sanitize: 'test...  ') equals: 'test'.
	self assert: (FileNameSanitizer sanitize: 'test. .st') equals: 'test.st'
]

{ #category : 'tests' }
FileNameSanitizerTest >> testSanitizeWindowsReservedNames [
	#('CON' 'prn' 'Aux' 'NUL' 'com1' 'COM9' 'lpt1' 'LPT9') do: [ :reserved |
		| sanitized |
		sanitized := FileNameSanitizer sanitize: reserved.
		self assert: sanitized equals: '_' , reserved ]
]

{ #category : 'tests' }
FileNameSanitizerTest >> testSanitizeWindowsReservedNamesWithExtension [
	self assert: (FileNameSanitizer sanitize: 'CON.st') equals: '_CON.st'.
	self assert: (FileNameSanitizer sanitize: 'aux.txt') equals: '_aux.txt'
]
