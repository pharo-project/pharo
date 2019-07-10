Extension { #name : #DateAndTimeTest }

{ #category : #'*Kernel-Tests-Extended' }
DateAndTimeTest >> testAsDateAndTime [
	| localOffset localOffsetSuffix |
	#('-1199-01-05T20:33:14.321-05:00' 
		'2002-05-16T17:20:45.1+01:01' 
		'2002-05-16T17:20:45.02+01:01' 
		'2002-05-16T17:20:45.003+01:01' 
		'2002-05-16T17:20:45.0004+01:01'
		'2002-05-16T17:20:45+01:57' 
		'2002-05-16T17:20:45.000006+01:01' 
		'2002-05-16T17:20:45.0000007+01:01' 
		'2002-05-16T17:20:45.00000008-01:01'    
		'2002-05-16T17:20:45.000000009+01:01'
 		'2002-05-16T17:20:45+00:00'		 
 		'2002-05-16T17:20:45-02:34'
		'1997-04-26T01:02:03+01:02:3') do: [ :each | 
			self assert: each asDateAndTime printString equals: each ].
		
	localOffset := DateAndTime localOffset.
	localOffsetSuffix := String streamContents: [ :out |
		out nextPut: (localOffset positive ifTrue: [ $+ ] ifFalse: [ $- ]).
		localOffset hours abs printOn: out base: 10 length: 2 padded: true.
		out nextPut: $:.
		localOffset minutes abs printOn: out base: 10 length: 2 padded: true.
		localOffset seconds = 0 ifFalse: [ out nextPut: $: ; print: localOffset seconds rounded abs ] ].
	
	#('2002-05-16T17:20:45.00005' 
		'2002-05-16T17:20:00' 
		'2002-05-16T17:20:45') do: [ :each | 
			self assert: each asDateAndTime printString equals: each , localOffsetSuffix ].
]
