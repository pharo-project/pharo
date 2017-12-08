Class {
	#name : #AbstractShiftChangesFicusTest,
	#superclass : #TestCase,
	#instVars : [
		'results'
	],
	#category : 'Shift-Changes-Ficus-Tests'
}

{ #category : #'accessing ficus classes' }
AbstractShiftChangesFicusTest >> ficusPackage [
	^ (MCWorkingCopy forPackage: (MCPackage named: #'Shift-Changes-Ficus-Tests')) ficusPackage
]

{ #category : #'accessing ficus classes' }
AbstractShiftChangesFicusTest >> lookForFicusClass: aName [
	^ self ficusPackage classes detect:[:e| e name = aName]
]

{ #category : #'accessing ficus classes' }
AbstractShiftChangesFicusTest >> shFTestClassA [
	^ self lookForFicusClass: #ShFTestClassA
]

{ #category : #'accessing ficus classes' }
AbstractShiftChangesFicusTest >> shFTestClassANew [
	^ self lookForFicusClass: #ShFTestClassANew
]

{ #category : #'accessing ficus classes' }
AbstractShiftChangesFicusTest >> shFTestClassAWithClassPoolEntry [
	^ self lookForFicusClass: #ShFTestClassAWithClassPoolEntry
]

{ #category : #'accessing ficus classes' }
AbstractShiftChangesFicusTest >> shFTestClassAWithClassVariable [
	^ self lookForFicusClass: #ShFTestClassAWithClassVariable
]

{ #category : #'accessing ficus classes' }
AbstractShiftChangesFicusTest >> shFTestClassAWithComment [
	^ self lookForFicusClass: #ShFTestClassAWithComment
]

{ #category : #'accessing ficus classes' }
AbstractShiftChangesFicusTest >> shFTestClassAWithSharedPool [
	^ self lookForFicusClass: #ShFTestClassAWithSharedPool
]

{ #category : #'accessing ficus classes' }
AbstractShiftChangesFicusTest >> shFTestClassB [
	^ self lookForFicusClass: #ShFTestClassB
]

{ #category : #'accessing ficus classes' }
AbstractShiftChangesFicusTest >> shFTestClassC [
	^ self lookForFicusClass: #ShFTestClassC
]

{ #category : #'accessing ficus classes' }
AbstractShiftChangesFicusTest >> shFTestClassCWithoutTrait [
	^ self lookForFicusClass: #ShFTestClassCWithoutTrait
]

{ #category : #'accessing ficus classes' }
AbstractShiftChangesFicusTest >> shFTestClassWithByteLayout [
	^ self lookForFicusClass: #ShFTestClassWithByteLayout
]

{ #category : #'accessing ficus classes' }
AbstractShiftChangesFicusTest >> shFTestClassWithDifferentSlotsNew [
	^ self lookForFicusClass: #ShFTestClassWithDifferentSlotsNew
]

{ #category : #'accessing ficus classes' }
AbstractShiftChangesFicusTest >> shFTestClassWithDifferentSlotsOld [
	^ self lookForFicusClass: #ShFTestClassWithDifferentSlotsOld
]
