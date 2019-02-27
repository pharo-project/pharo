Class {
	#name : #MorphicListCommonPropertiestTest,
	#superclass : #MorphicAbstractListCommonPropertiestTest,
	#category : #'Spec-MorphicBackendTests'
}

{ #category : #running }
MorphicListCommonPropertiestTest >> classToTest [
	^ ListPresenter
]

{ #category : #running }
MorphicListCommonPropertiestTest >> testRemoveHeaderTitleInPresenterRemovesColumnHeaderMorph [

	| tableContainer |
	
	SystemVersion current major = 7
		ifTrue: [
			"Test failing in Pharo7 due to a bug in FastTable"
			^ self skip ].
	
	self presenter headerTitle: 'test'.
	World doOneCycle.

	self presenter hideHeaderTitle.
	World doOneCycle.

	tableContainer := self widget submorphs first.
	self assert: tableContainer submorphs size equals: 3
]

{ #category : #running }
MorphicListCommonPropertiestTest >> testSetColumnTitleInPresenterPutsColumnHeaderMorph [

	| tableContainer tableRowMorph cellMorph |
	self presenter headerTitle: 'test'.

	tableContainer := self widget submorphs first.
	tableRowMorph := tableContainer submorphs first.
	cellMorph := tableRowMorph submorphs first.
	self assert: cellMorph submorphs first contents equals: 'test'
]

{ #category : #running }
MorphicListCommonPropertiestTest >> testSetColumnTitleInPresenterShowsThemInWidget [

	self presenter headerTitle: 'test'.
	
	self assert: self widget isShowColumnHeaders
]
