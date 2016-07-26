accessing comment
classCommentBlank

	| existingComment stream |
	existingComment := self theNonMetaClass organization classComment.
	existingComment isEmpty
		ifFalse: [^existingComment].

	stream := (String new: 100) writeStream.
	
	stream nextPutAll: 'Please comment me using the following template inspired by Class Responsibility Collaborator (CRC) design:

For the Class part:  State a one line summary. For example, "I represent a paragraph of text".

For the Responsibility part: Three sentences about my main responsibilities - what I do, what I know.

For the Collaborators Part: State my main collaborators and one line about how I interact with them. 

Public API and Key Messages

- message one   
- message two 
- (for bonus points) how to create instances.

   One simple example is simply gorgeous.
 
Internal Representation and Key Implementation Points.'.
			
	(self instVarNames size > 0)
		 ifTrue: [stream cr; cr; nextPutAll: '    Instance Variables'.  ].
		
	self instVarNames asSortedCollection do: [:each |
		stream
			cr; tab; nextPutAll: each;
			nextPut: $:;
			tab; tab;
			nextPutAll: '<Object>'].
	stream cr.
stream cr; cr; nextPutAll: '    Implementation Points'.
	^stream contents