Class {
	#name : 'PackageTopoSortTest',
	#superclass : 'PackageTestCase',
	#category : 'Kernel-CodeModel-Tests-Packages',
	#package : 'Kernel-CodeModel-Tests',
	#tag : 'Packages'
}

{ #category : 'tests' }
PackageTopoSortTest >> testSortClassAndSubclassPutsSubclassLast [

	| sorted |
	sorted := Package new sortTopologically: {
			          PackageTopoSortTest superclass.
			          PackageTopoSortTest }.
	
	self assert: sorted asArray equals: {
			PackageTopoSortTest superclass.
			PackageTopoSortTest }
]

{ #category : 'tests' }
PackageTopoSortTest >> testSortEmptyArray [

	| sorted |
	sorted := Package new sortTopologically: #().
	self assert: sorted isEmpty
]

{ #category : 'tests' }
PackageTopoSortTest >> testSortSubclassAndClassPutsSubclassLast [

	| sorted |
	sorted := Package new sortTopologically: {
			          PackageTopoSortTest.
			          PackageTopoSortTest superclass }.

	self assert: sorted asArray equals: {
			PackageTopoSortTest superclass.
			PackageTopoSortTest }
]

{ #category : 'tests' }
PackageTopoSortTest >> testSortUnrelatedClassesDoesNotMatterOrder [

	| sorted |
	sorted := Package new sortTopologically: { PackageTopoSortTest . PackageTest }.
	self assertCollection: sorted asSet hasSameElements: { PackageTopoSortTest . PackageTest }
]
