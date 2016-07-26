accessing
keysSortedSafely
	"Answer an Array containing the receiver's keys."
 	"Suggested by l. Uzonyi"
	
 	| sortedKeys |
 	sortedKeys := Array 
						new: self size 
						streamContents: [ :stream |
 								self keysDo: [ :each | stream nextPut: each ] ].
 	sortedKeys sort: [ :x :y |
 		"Should really be use <obj, string, num> compareSafely..."
 		((x isString and: [ y isString ])
 			or: [ x isNumber and: [ y isNumber ] ])
 			ifTrue: [ x < y ]
 			ifFalse: [ x class == y class
 				ifTrue: [ x printString < y printString ]
 				ifFalse: [ x class name < y class name ] ] ].
 	^sortedKeys