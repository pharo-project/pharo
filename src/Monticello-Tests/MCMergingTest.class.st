Class {
	#name : 'MCMergingTest',
	#superclass : 'MCTestCase',
	#instVars : [
		'conflicts'
	],
	#category : 'Monticello-Tests-Base',
	#package : 'Monticello-Tests',
	#tag : 'Base'
}

{ #category : 'asserting' }
MCMergingTest >> assert: aCollection hasElements: anArray [

	self assert: (aCollection collect: [ :ea | ea token ]) asSet equals: anArray asSet
]

{ #category : 'asserting' }
MCMergingTest >> assertMerge: local with: remote base: ancestor gives: result conflicts: conflictResult [
	| merger |
	conflicts := #().
	merger := MCThreeWayMerger base: (self snapshotWithElements: local) target: (self snapshotWithElements: remote) ancestor: (self snapshotWithElements: ancestor).
	merger conflicts do: [ :ea | self handleConflict: ea ].
	self assert: merger mergedSnapshot definitions hasElements: result.
	self assert: conflicts asSet equals: conflictResult asSet
]

{ #category : 'emulating' }
MCMergingTest >> handleConflict: aConflict [	
	|l r|
	l := #removed.
	r := #removed.
	aConflict baseDefinition ifNotNil: [:d | l := d token].
	aConflict targetDefinition ifNotNil: [:d | r := d token].	
	conflicts := conflicts copyWith: (Array with: r with: l).
	(l = #removed or: [r = #removed])
		ifTrue: [aConflict chooseRemote]
		ifFalse:
			[l > r
				ifTrue: [aConflict chooseLocal]
				ifFalse: [aConflict chooseRemote]]
		
]

{ #category : 'emulating' }
MCMergingTest >> snapshotWithElements: anArray [

	^ MCSnapshot
		fromDefinitions: (anArray collect: [:t | self mockToken: t])
]

{ #category : 'tests' }
MCMergingTest >> testAdditiveConflictlessMerge [
	self
		assertMerge: #(a1 b1)
				with: #(a1 c1)
				base: #(a1)
			
				gives: #(a1 b1 c1)
				conflicts: #()
]

{ #category : 'tests' }
MCMergingTest >> testComplexConflictlessMerge [
	self 
		assertMerge: #(a1 b1 d1)
				with: #(a2 c1)
				base: #(a1 c1 d1)
				
				gives: #(a2 b1)
				conflicts: #()
]

{ #category : 'tests' }
MCMergingTest >> testIdenticalModification [
	self
		assertMerge: #(a2 b1)
				with: #(a2 b1)
				base: #(a1 b1)
				
				gives: #(a2 b1)
				conflicts: #()
]

{ #category : 'tests' }
MCMergingTest >> testLocalModifyRemoteRemove [
	self assertMerge: #(a2 b1)
				with: #(b1)
				base: #(a1 b1)
				
				gives: #(b1)
				conflicts: #((removed a2)).
				
	self assertMerge: #(a1 b1)
				with: #(b1)
				base: #(a2 b1)
				
				gives: #(b1)
				conflicts: #((removed a1)).
]

{ #category : 'tests' }
MCMergingTest >> testLocalRemoveRemoteModify [
	self assertMerge: #(b1)
				with: #(a1 b1)
				base: #(a2 b1)
				
				gives: #(a1 b1)
				conflicts: #((a1 removed)).

	self assertMerge: #(b1)
				with: #(a2 b1)
				base: #(a1 b1)
				
				gives: #(a2 b1)
				conflicts: #((a2 removed)).
]

{ #category : 'tests' }
MCMergingTest >> testMultiPackageMerge [
	| merger |
	conflicts := #().
	merger := MCThreeWayMerger new.
	merger addBaseSnapshot: (self snapshotWithElements: #(a1 b1)).
	merger applyPatch: ((self snapshotWithElements: #()) patchRelativeToBase: (self snapshotWithElements: #(a1))).
	merger applyPatch: ((self snapshotWithElements: #(a2 b1)) patchRelativeToBase: (self snapshotWithElements: #(b1))).
	merger conflicts do: [:ea | self handleConflict: ea].
	self assert: merger mergedSnapshot definitions hasElements: #(a2 b1).
	self assertEmpty: conflicts
]

{ #category : 'tests' }
MCMergingTest >> testMultiPackageMerge2 [
	| merger |
	conflicts := #().
	merger := MCThreeWayMerger new.
	merger addBaseSnapshot: (self snapshotWithElements: #(a1 b1)).
	merger applyPatch: ((self snapshotWithElements: #()) patchRelativeToBase: (self snapshotWithElements: #(a1))).
	merger applyPatch: ((self snapshotWithElements: #(a1 b1)) patchRelativeToBase: (self snapshotWithElements: #(b1))).
	merger conflicts do: [:ea | self handleConflict: ea].
	self assert: merger mergedSnapshot definitions hasElements: #(a1 b1).
	self assertEmpty: conflicts
]

{ #category : 'tests' }
MCMergingTest >> testMultiPackageMerge3 [
	| merger |
	conflicts := #().
	merger := MCThreeWayMerger new.
	merger addBaseSnapshot: (self snapshotWithElements: #(a1 b1)).
	merger applyPatch: ((self snapshotWithElements: #(a1 b1)) patchRelativeToBase: (self snapshotWithElements: #(b1))).
	merger applyPatch: ((self snapshotWithElements: #()) patchRelativeToBase: (self snapshotWithElements: #(a1))).
	merger conflicts do: [:ea | self handleConflict: ea].
	self assert: merger mergedSnapshot definitions hasElements: #(a1 b1).
	self assertEmpty: conflicts
]

{ #category : 'tests' }
MCMergingTest >> testMultipleConflicts [
	self assertMerge: #(a1 b3 c1)
				with: #(a1 b2 d1)
				base: #(a1 b1 c2)
				
				gives: #(a1 b3 d1)
				conflicts: #((removed c1) (b2 b3))

]

{ #category : 'tests' }
MCMergingTest >> testSimultaneousModification [
	self assertMerge: #(a2)
				with: #(a3)
				base: #(a1)
				
				gives: #(a3)
				conflicts: #((a3 a2)).
]

{ #category : 'tests' }
MCMergingTest >> testSimultaneousRemove [
	self assertMerge: #(a1)
				with: #(a1)
				base: #(a1 b1)
				
				gives: #(a1)
				conflicts: #()
]

{ #category : 'tests' }
MCMergingTest >> testSubtractiveConflictlessMerge [
	self assertMerge: #(a1 b1)
				with: #()
				base: #(a1)
				
				gives: #(b1)
				conflicts: #()
]
