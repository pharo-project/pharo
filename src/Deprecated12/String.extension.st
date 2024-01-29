Extension { #name : 'String' }

{ #category : '*Deprecated12' }
String >> asUncommentedCode [
	"this string represents a commented code, let's uncomment it"

	self deprecated: 'This method will be removed in the future version of Pharo because the implementation does not seem to have use cases or there are too few.'.

	"'""abc""' asUncommentedCode >>> 'abc'"
	
	^ String streamContents:  [ :str |

		|doubleQuoteAlreadyFound|
		doubleQuoteAlreadyFound := false.

		self withoutQuoting do: [ :char |
			char = $"
				ifTrue: [
					doubleQuoteAlreadyFound 	ifTrue: [
							str nextPut: $" ].
					doubleQuoteAlreadyFound := doubleQuoteAlreadyFound not.
				]
				ifFalse: [
					str nextPut: char
				]
		 ]
	]
]

{ #category : '*Deprecated12' }
String class >> crlfcrlf [

	self
		deprecated: 'Will be removed without replacement in next iteration'
		on: '21 September 2023'
		in:
		'Pharo-12.0.0+build.779.sha.e06ae6a0b17ea62c2031af00a73fd2471d563666 (64 Bit)'.
	^ self crlf , self crlf
]

{ #category : '*Deprecated12' }
String >> findCloseParenthesisFor: startIndex [
	"assume (self at: startIndex) is $(.  Find the matching $), allowing parentheses to nest."
	" '(1+(2-3))-3.14159' findCloseParenthesisFor: 1 "
	" '(1+(2-3))-3.14159' findCloseParenthesisFor: 4 "

	| result |
	self
		deprecated:
		'Use #findClosing:startingAt: instead, it is faster and more generic. Note that this new method returns 0 if the closing character is not found.'
		transformWith: '`@receiver findCloseParenthesisFor: `@arg'
			-> '`@receiver findClosing: $) startingAt: `@arg'.
	result := self findClosing: $) startingAt: startIndex.
	^ result = 0
		  ifTrue: [ self size + 1 ]
		  ifFalse: [ result ]
]
