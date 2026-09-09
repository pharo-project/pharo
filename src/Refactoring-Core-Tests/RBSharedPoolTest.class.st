Class {
	#name : 'RBSharedPoolTest',
	#superclass : 'RBRefactoringBrowserTest',
	#instVars : [
		'rbNamespace',
		'objectClass',
		'messageNodeClass',
		'userOf',
		'defineOf'
	],
	#category : 'Refactoring-Core-Tests-Model',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Model'
}

{ #category : 'running' }
RBSharedPoolTest >> setUp [

	super setUp.
	rbNamespace := RBNamespace new.
	objectClass := rbNamespace classNamed: #Object.
	messageNodeClass := rbNamespace classNamed: #OCMessageNode.

	rbNamespace defineClass: [ :aBuilder |
		aBuilder
			superclass: SharedPool;
			name: #ChronologyConstants;
			sharedVariables:
				{ #DayNames. #DaysInMonth. #HoursInDay. #MicrosecondsInDay. #MinutesInHour. #MonthNames. #NanosInMillisecond. #NanosInSecond. #SecondsInDay. #SecondsInHour.
				#SecondsInMinute. #Epoch };
			package: 'Kernel-Chronology' ].

	defineOf := rbNamespace classNamed: #ChronologyConstants.

	rbNamespace defineClass: [ :aBuilder |
		aBuilder
			superclass: Magnitude;
			name: #DateAndTime;
			slots: { #seconds. #offset. #julianDayNumber. #nanos };
			sharedVariables: { #ClockProvider. #LocalTimeZoneCache };
			sharedPools: #(ChronologyConstants);
			package: 'Kernel-Chronology' ].

	userOf := rbNamespace classNamed: #DateAndTime
]

{ #category : 'method tests' }
RBSharedPoolTest >> testDefinesClassVariable [

	self assert: (defineOf definesClassVariable: #DayNames)
]

{ #category : 'method tests' }
RBSharedPoolTest >> testDefinesPoolDictionary [
	"Pay attention that definesPoolDictionary: is badly named, because in fact
	it checks whether a class uses a sharedPool!!!!"

	self assert: (userOf definesPoolDictionary: #ChronologyConstants)
]

{ #category : 'method tests' }
RBSharedPoolTest >> testIsNotSharedPool [

	self deny: objectClass isSharedPool
]

{ #category : 'method tests' }
RBSharedPoolTest >> testIsSharedPool [

	self assert: defineOf isSharedPool
]

{ #category : 'method tests' }
RBSharedPoolTest >> testRemovePoolDictionary [
	"Pay attention that definesPoolDictionary: is badly named, because in fact
	it checks whether a class uses a sharedPool!!!!"

	(userOf removePoolDictionary: #ChronologyConstants).
	self deny: (userOf definesPoolDictionary: #ChronologyConstants ).

	self should: [ (userOf removePoolDictionary: #ChronologyConstants) ] raise: NotFound
]
