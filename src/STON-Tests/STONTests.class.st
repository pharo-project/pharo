"
STONTests tests the API offered by STON.

"
Class {
	#name : #STONTests,
	#superclass : #TestCase,
	#category : 'STON-Tests'
}

{ #category : #utilities }
STONTests class >> fastReadFromFileNamed: path [
	^ path asFileReference
		readStreamDo: [ :stream | 
			STON reader
				on: (ZnBufferedReadStream on: stream);
				optimizeForLargeStructures;
				next ]
]

{ #category : #utilities }
STONTests class >> fastWrite: object toFileNamed: path [
	^ path asFileReference
		writeStreamDo: [ :fileStream | 
			ZnBufferedWriteStream
				on: fileStream
				do: [ :stream | 
					STON writer
						on: stream;
						optimizeForLargeStructures;
						nextPut: object ] ]
]

{ #category : #utilities }
STONTests class >> readFromFileNamed: path [
	^ path asFileReference
		readStreamDo: [ :stream | 
			STON reader
				on: stream;
				next ]
]

{ #category : #utilities }
STONTests class >> write: object toFileNamed: path [
	^ path asFileReference
		writeStreamDo: [ :stream | 
			STON writer
				on: stream;
				nextPut: object ]
]

{ #category : #tests }
STONTests >> testFromString [
	| object |
	object := STON listClass withAll: { 1. 0. -1. true. false. nil }.
	self assert: (STON fromString: '[1,0,-1,true,false,nil]') = object
]

{ #category : #tests }
STONTests >> testFromStringWithComments [
	| object |
	object := STON listClass withAll: { 1. 0. -1. true. false. nil }.
	self 
		assert: (STON fromStringWithComments: '// comment\[1, /* comment */ 0, -1, true, false, nil] // comment' withCRs)
		equals: object
]

{ #category : #tests }
STONTests >> testPrettyPrinting [
	| object |
	object := STONTestUser dummy.
	self assert: (STON fromString: (STON toStringPretty: object)) = object.
	object := STONTestDomainObject dummy.
	self assert: (STON fromString: (STON toStringPretty: object)) = object
]

{ #category : #tests }
STONTests >> testRoomExitCycles [
	| model room1 room2 exit1 exit2 ston object |
	(room1 := STONTestMap new) at: #name put: 'Room 1'.
	(room2 := STONTestMap new) at: #name put: 'Room 2'.
	(exit1 := STONTestMap new)
		at: #name put: 'Exit 1';
		at: #origin put: room1;
		at: #destination put: room2.
	(exit2 := STONTestMap new)
		at: #name put: 'Exit 2';
		at: #origin put: room2;
		at: #destination put: room1.
	room1 at: #exit put: exit1.
	room2 at: #exit put: exit2.
	model := Array with: room1 with: room2.
	ston := STON toString: model.
	object := STON fromString: ston.
	"We can't just compare because this is a recursive datastructure"
	self assert: (object first at: #name) equals: 'Room 1'.
	self assert: (object second at: #name) equals: 'Room 2'.
	self assert: ((object first at: #exit) at: #name) equals: 'Exit 1'.
	self assert: ((object second at: #exit) at: #name) equals: 'Exit 2'.
	self assert: ((object first at: #exit) at: #origin) == object first.
	self assert: ((object first at: #exit) at: #destination) == object second.
	self assert: ((object second at: #exit) at: #origin) == object second.
	self assert: ((object second at: #exit) at: #destination) == object first.
	"Try writing again the parse model" 
	self assert: (STON toString: object) equals: ston
]

{ #category : #tests }
STONTests >> testToString [
	| object |
	object := STON listClass withAll: { 1. 0. -1. true. false. nil }.
	self assert: (STON toString: object) = '[1,0,-1,true,false,nil]'
]
