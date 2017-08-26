debugger access
errorReportOn: stream
	"Write a detailed error report on the stack (above me) on a  
	stream.  For both the error file, and emailing a bug report.   
	Suppress any errors while getting printStrings.  Limit the length."
	
	| stackDepth aContext |
	stream print: Date today; space; print: Time now; cr.
	stream cr.
	stream nextPutAll: 'VM: ';
		nextPutAll: Smalltalk os name asString;
		nextPutAll: ' - ';	
		nextPutAll: Smalltalk os subtype asString;
		nextPutAll: ' - ';
		nextPutAll: Smalltalk os version asString;
		nextPutAll: ' - ';
		nextPutAll: Smalltalk vm version asString;
		cr.	
	stream nextPutAll: 'Image: ';
		nextPutAll:  SystemVersion current version asString;
		nextPutAll: ' [';
		nextPutAll: Smalltalk lastUpdateString asString;
		nextPutAll: ']';
		cr.
	stream cr.
	"Note: The following is an open-coded version of  Context>>stackOfSize: since this method may be called during a  low space condition and we might run out of space for allocating the  full stack."	
	stackDepth := 0.  
	aContext := self.
	[ aContext notNil and: [ (stackDepth := stackDepth + 1) < 40 ]] 
		whileTrue: [
			"variable values"
			aContext printDetails: stream.
			stream cr.
			aContext := aContext sender ].
	stream cr; nextPutAll: '--- The full stack ---'; cr.
	aContext := self.	
	stackDepth := 0.
	[ aContext == nil ] whileFalse:	[
		stackDepth := stackDepth + 1.	
		stackDepth = 40 ifTrue: [ stream nextPutAll: ' - - - - - - - - - - - - - - -  
			- - - - - - - - - - - - - - - - - -'; cr ].
		"just class>>selector"	
		stream print: aContext; cr. 		
		stackDepth > 200 ifTrue: [
			stream nextPutAll: '-- and more not shown --'.
			^  self ].
		aContext := aContext sender ].