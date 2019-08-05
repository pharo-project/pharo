"
Tally for assembling system profiles. It's a subclass of Link so we can chain tallies together in the receivers list.
"
Class {
	#name : #QSystemTally,
	#superclass : #Link,
	#instVars : [
		'class',
		'method',
		'receivers',
		'tally',
		'senders'
	],
	#category : #'Tool-Profilers-System'
}

{ #category : #LICENSE }
QSystemTally class >> LICENSE [
	^'Project Squeak

	In Memory of Andreas Raab.  Author, Friend, Colleague. 	http://forum.world.st/In-Memory-of-Andreas-Raab-td4663424.html
	Copyright (c) 2005-2013, 3D Immersive Collaboration Consulting, LLC., All Rights Reserved

	Redistributions in source code form must reproduce the above
	copyright and this condition.

Licensed under MIT License (MIT)
Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'
]

{ #category : #converting }
QSystemTally >> asArray [
	| link |
	^Array streamContents: [ :s |
		link := self.
		[link == nil] whileFalse: [
			s nextPut: link.
			link := link nextLink.
		].
	].
]

{ #category : #converting }
QSystemTally >> asSortedCollection: aCollection [
	"Create a sorted collection from the given input"
	^aCollection asSortedCollection: [ :tA :tB | tA tally >= tB tally ]
]

{ #category : #report }
QSystemTally >> bump: hitCount fromSender: senderTally [
	"Add this hitCount to the total, and include a reference to the
	sender responsible for the increment"
	self bumpBy: hitCount.
	senders ifNil: [ senders := OrderedCollection new ].
	senderTally ifNotNil: [ senders add: (senderTally copyWithTally: hitCount) ]
]

{ #category : #tallying }
QSystemTally >> bumpBy: count [
	"Bump this tally by the specified amount"
	tally := tally + count
]

{ #category : #initialize }
QSystemTally >> class: aClass method: aCompiledMethod [
	class := aClass.
	method := aCompiledMethod.
	tally := 0.
]

{ #category : #copying }
QSystemTally >> copyWithTally: hitCount [
	^ (QSystemTally new class: class method: method) bumpBy: hitCount
]

{ #category : #report }
QSystemTally >> fullPrintOn: aStream tallyExact: isExact orThreshold: perCent time: totalTime [
	| threshold |  
	isExact ifFalse: [threshold := (perCent asFloat / 100 * tally) rounded].
	aStream nextPutAll: '**Tree**'; cr.
	self treePrintOn: aStream
		tabs: OrderedCollection new
		thisTab: ''
		total: tally
		totalTime: totalTime
		tallyExact: isExact
		orThreshold: threshold.
	aStream nextPut: Character newPage; cr.
	aStream nextPutAll: '**Leaves**'; cr.
	self leavesPrintOn: aStream
		tallyExact: isExact
		orThreshold: threshold
		time: totalTime.
]

{ #category : #report }
QSystemTally >> getNewTabsFor: tabs [
	^ tabs size < self maxTabs
		ifTrue: [ tabs ]
		ifFalse: [ (tabs select: [ :x | x = '[' ]) copyWith: '[' ]
]

{ #category : #report }
QSystemTally >> into: leafDict fromSender: senderTally [
	| leafNode |
	leafNode := leafDict at: method ifAbsentPut: [ QSystemTally new class: class method: method ].
	leafNode bump: tally fromSender: senderTally
]

{ #category : #testing }
QSystemTally >> isPrimitives [
	"Detect pseudo node used to carry tally of local hits"

	^ method isNil
]

{ #category : #report }
QSystemTally >> leavesInto: leafDict fromSender: senderTally [
	| rcvrs |
	rcvrs := self sonsOver: 0.
	rcvrs size = 0
		ifTrue: [self into: leafDict fromSender: senderTally]
		ifFalse: [rcvrs do:[:node |
				node isPrimitives
					ifTrue: [node leavesInto: leafDict fromSender: senderTally]
					ifFalse: [node leavesInto: leafDict fromSender: self]]]
]

{ #category : #report }
QSystemTally >> leavesPrintOn: aStream tallyExact: isExact orThreshold: threshold time: totalTime [
	"@TODO: Apparently, providing true as isExact will always produce an error "
	| dict |
	dict := IdentityDictionary new: 100.
	self leavesInto: dict fromSender: nil.
	isExact 
		ifTrue: [
			(self asSortedCollection: dict) do: [ :node |
				node printOn: aStream total: tally totalTime: nil tallyExact: isExact.
				node printSenderCountsOn: aStream ] ] 
		ifFalse:[
			((self asSortedCollection: dict)
				select: [ :node | node tally > threshold ]) do: [ :node |
					node printOn: aStream total: tally totalTime: totalTime tallyExact: isExact ] ]
]

{ #category : #accessing }
QSystemTally >> maxClassNameSize [
	"Return the default maximum width of the class name alone"
	^30
]

{ #category : #accessing }
QSystemTally >> maxClassPlusSelectorSize [
	"Return the default maximum width of the class plus selector together (not counting the '>>')"
	^60
]

{ #category : #accessing }
QSystemTally >> maxTabs [
	"Return the default number of tabs after which leading white space is compressed"
	^36
]

{ #category : #accessing }
QSystemTally >> method [
	"Answer the CompiledMethod associated with this tally"
	^method
]

{ #category : #initialize }
QSystemTally >> primitives: anInteger [
	"Make the receiver be a node of unassigned primitives"
	tally := anInteger.
	method := nil. "indicates primitives"
]

{ #category : #printing }
QSystemTally >> printOn: aStream [
	aStream print: class; nextPutAll: '>>'; print: (method ifNotNil:[method selector]).
	aStream nextPutAll: ' -- '; print: tally.
]

{ #category : #report }
QSystemTally >> printOn: aStream total: total totalTime: totalTime tallyExact: isExact [ 
	| className myTally aClass percentage |
	isExact ifTrue:[
		myTally := tally.
		receivers ifNotNil: [ receivers asArray do: [ :r | myTally := myTally - r tally ] ].
		aStream print: myTally; space.
	] ifFalse:[
		percentage := tally asFloat / total * 100.0 roundTo: 0.1.
		aStream print: percentage; nextPutAll: ' (';
				nextPutAll: (percentage * totalTime / 100) rounded asStringWithCommas;
				nextPutAll: ')  '.
	].
	self isPrimitives ifTrue:[
		aStream nextPutAll: 'primitives'; cr
	] ifFalse:[
		aClass := method methodClass.
		className := aClass name contractTo: self maxClassNameSize.
		aStream nextPutAll: class name;
				nextPutAll: (aClass = class 
							ifTrue: ['  ']
							ifFalse: [' [' , aClass name , ']  ']);
				nextPutAll: (method selector
							contractTo: self maxClassPlusSelectorSize - className size);
				cr.
	].
]

{ #category : #report }
QSystemTally >> sonsOver: threshold [
	"Answer the sons with tallys over the given threshold"
	"threshold is a number "
	receivers ifNil: [ ^ #() ].
	^ receivers asArray select: [ :son | son tally > threshold ].
]

{ #category : #accessing }
QSystemTally >> tally [
	"Answer the tally count for this node"
	^tally
]

{ #category : #tallying }
QSystemTally >> tally: context by: count [
	"Explicitly tally the specified context and its stack."
	| root |
	(root := context  sender) 
		ifNil:[^(self bumpBy: count) tallyPath: context by: count]
		ifNotNil:[^(self tally: root by: count) tallyPath: context by: count]
]

{ #category : #tallying }
QSystemTally >> tallyMethod: aMethod by: count [
	"Called explicitly and needs to decrement receiver's tally count"
	| node |
	node := receivers.
	[node isNil] whileFalse:[
		node method == aMethod ifTrue:[^node bumpBy: count].
		node := node nextLink.
	].
	node := QSystemTally new class: aMethod methodClass method: aMethod.
	node nextLink: receivers.
	receivers := node.
	^node bumpBy: count
]

{ #category : #tallying }
QSystemTally >> tallyPath: context by: count [
	"Tally the context chain"

	| aMethod aTally |
	aMethod := context method.
	aTally := receivers.
	[ aTally isNil ]
		whileFalse: [ 
			aTally method == aMethod
				ifTrue: [ ^ aTally bumpBy: count ].
			aTally := aTally nextLink ].
	aTally := QSystemTally new class: context receiver class method: aMethod.
	aTally nextLink: receivers.
	receivers := aTally.
	^ aTally bumpBy: count
]

{ #category : #report }
QSystemTally >> treePrintOn: aStream tabs: tabs thisTab: myTab total: total totalTime: totalTime tallyExact: isExact orThreshold: threshold [

	"aStream contains the output of the print "

	"tabs is a collection of strings "

	| sons sonTab |

	tabs do: [ :tab | aStream nextPutAll: tab ].
	tabs size > 0
		ifTrue: [ self
				printOn: aStream
				total: total
				totalTime: totalTime
				tallyExact: isExact
			].
	sons := isExact
		ifTrue: [ receivers ]
		ifFalse: [ self sonsOver: threshold ].
	sons ifNil: [ ^ self ].
	sons notEmpty
		ifTrue: [ tabs addLast: myTab.
			sons := self asSortedCollection: sons.
			1 to: sons size do: [ :i | 
				sonTab := i < sons size
					ifTrue: [ '  |' ]
					ifFalse: [ '  ' ].
				( sons at: i )
					treePrintOn: aStream
					tabs: ( self getNewTabsFor: tabs )
					thisTab: sonTab
					total: total
					totalTime: totalTime
					tallyExact: isExact
					orThreshold: threshold
				].
			tabs removeLast
			]
]
