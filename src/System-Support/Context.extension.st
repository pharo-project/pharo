Extension { #name : 'Context' }

{ #category : '*System-Support' }
Context >> errorReportOn: stream [
	"Write a detailed error report on the stack (above me) on a
	stream.  For both the error file, and emailing a bug report.
	Suppress any errors while getting printStrings.  Limit the length."

	| stackDepth aContext |
	"The packae System-Time is not loaded during the first steps of the bootstrap. Thus we print the date only if we have the necessary classes loaded in the system. Else we at least print the useful info."
	self class environment at: #Date ifPresent: [ :date |
			self class environment at: #Time ifPresent: [ :time |
				[ stream
						print: date today;
						space;
						print: time now;
						cr ] onErrorDo: [ :err | stream nextPutAll: '<< date and time not available >>'; cr ] ] ].

	stream cr.
	stream
		nextPutAll: 'VM: ';
		nextPutAll: OSPlatform current name;
		nextPutAll: ' - ';
		nextPutAll: OSPlatform current subtype;
		nextPutAll: ' - ';
		nextPutAll: OSPlatform current version;
		nextPutAll: ' - ';
		nextPutAll: Smalltalk vm version;
		cr.
	
	self class environment at: #SystemVersion ifPresent: [ :class |
		stream
			nextPutAll: 'Image: ';
			print: class current;
			cr.
 ].

	stream cr. "Note: The following is an open-coded version of  Context>>stackOfSize: since this method may be called during a  low space condition and we might run out of space for allocating the  full stack."
	stackDepth := 0.
	aContext := self.
	[ aContext isNotNil and: [ (stackDepth := stackDepth + 1) < 40 ] ] whileTrue: [ "variable values"
			aContext printDetails: stream.
			stream cr.
			aContext := aContext sender ].
	stream
		cr;
		nextPutAll: '--- The full stack ---';
		cr.
	aContext := self.
	stackDepth := 0.
	[ aContext == nil ] whileFalse: [
			stackDepth := stackDepth + 1.
			stackDepth = 40 ifTrue: [
					stream
						nextPutAll: ' - - - - - - - - - - - - - - -
			- - - - - - - - - - - - - - - - - -';
						cr ]. "just class>>selector"
			stream
				print: aContext;
				cr.
			stackDepth > 200 ifTrue: [
					stream nextPutAll: '-- and more not shown --'.
					^ self ].
			aContext := aContext sender ]
]

{ #category : '*System-Support' }
Context >> printDetails: stream [
	"Put my class>>selector and instance variables and arguments and temporaries on the stream.  Protect against errors during printing."

	| errorMessage string |
	self printOn: stream.
	stream cr.
	stream
		tab;
		nextPutAll: 'Receiver: '.
	errorMessage := '<<error during printing: '.
	stream nextPutAll: ([ receiver printStringLimitedTo: 90 ]
			 on: Exception
			 do: [ :err | errorMessage , err messageText asString , '>>' ]).
	stream
		cr;
		tab;
		nextPutAll: 'Receiver''s instance variables: ';
		cr.
	[
		receiver class allInstVarNames
			ifEmpty: [ stream nextPutAll: (receiver printStringLimitedTo: 90) ]
			ifNotEmpty: [ receiver longPrintOn: stream limitedTo: 80 indent: 2 ] ]
		on: Exception
		do: [ :err | stream nextPutAll: errorMessage , err messageText asString , '>>' ].

	stream
		cr;
		tab;
		nextPutAll: 'Arguments and temporary variables: ';
		cr.
	string := [ (self tempsAndValuesLimitedTo: 80 indent: 2) padRightTo: 1 with: $x ]
		          on: Exception
		          do: [ :ex | errorMessage , ex messageText asString , '>>' ].
	stream nextPutAll: string.
	stream cr
]

{ #category : '*System-Support' }
Context >> tempsAndValuesLimitedTo: sizeLimit indent: indent [
	"Return a string of the temporary variabls and their current values"

	"Compute a line limit to not go over @sizeLimit per line"
	| lineLimit |
	lineLimit := sizeLimit - 'temp0: ' size - indent.
	^ String streamContents: [ :aStream |
			  (1 to: self numTemps)
				  do: [ :index |
						  indent timesRepeat: [ aStream tab ].
						  aStream
							  nextPutAll: 'temp';
							  print: index;
							  nextPut: $:;
							  space;
							  tab.
						  aStream nextPutAll: ((self tempAt: index) printStringLimitedTo: 
						lineLimit) ]
				  separatedBy: [ aStream cr ] ]
]
