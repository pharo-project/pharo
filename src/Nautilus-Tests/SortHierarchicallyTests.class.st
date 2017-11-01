Class {
	#name : #SortHierarchicallyTests,
	#superclass : #TestCase,
	#instVars : [
		'completeTree',
		'partialTree',
		'unrelatedClasses',
		'archivesClasses'
	],
	#category : 'Nautilus-Tests'
}

{ #category : #private }
SortHierarchicallyTests >> nodes: nodes shouldBe: correctNodes [
	"sort the collection of nodes. The result should be the correctNodes"

	| sorted |
	sorted := SortHierarchically sortClasses: nodes .
	self assert: sorted size equals: correctNodes size.
	correctNodes with: sorted do: [:node :sortedNode | self assert: node equals: sortedNode]
]

{ #category : #running }
SortHierarchicallyTests >> setUp [

	completeTree := {String . ByteString . Symbol . ByteSymbol . WideSymbol . WideString} .
	partialTree := {String . ByteString . ByteSymbol . WideString}.
	unrelatedClasses :=  {ArrayedCollection . BlockClosure . Class  . Morph . Point}.
	archivesClasses :=  {Archive . TarArchive . ZipArchive . ArchiveMember . TarArchiveMember . ZipArchiveMember . ZipFileMember . ZipDirectoryMember . ZipNewFileMember . ZipStringMember . ZipFileConstants}
]

{ #category : #private }
SortHierarchicallyTests >> sortByNameSize: collection [

	^collection asSortedCollection: [:x :y | x name size < y name size]
]

{ #category : #tests }
SortHierarchicallyTests >> testArchivesClassesReversed [

	self
		nodes: archivesClasses reversed
		shouldBe: archivesClasses
]

{ #category : #tests }
SortHierarchicallyTests >> testArchivesClassesUnchanged [

	self
		nodes: archivesClasses
		shouldBe: archivesClasses
]

{ #category : #tests }
SortHierarchicallyTests >> testArchivesClassesUnordered [

	self
		nodes: (self sortByNameSize: archivesClasses)
		shouldBe: archivesClasses
		
	
]

{ #category : #tests }
SortHierarchicallyTests >> testCompleteTreeReversed [

	self
		nodes: completeTree reversed
		shouldBe: completeTree
]

{ #category : #tests }
SortHierarchicallyTests >> testCompleteTreeUnchanged [

	self
		nodes: completeTree
		shouldBe: completeTree
]

{ #category : #tests }
SortHierarchicallyTests >> testCompleteTreeUnordered [

	self
		nodes: (self sortByNameSize: completeTree)
		shouldBe: completeTree
]

{ #category : #tests }
SortHierarchicallyTests >> testOneClass [

	| pointNode sorted |
	pointNode := SortHierarchicallyIndentNode.
	sorted := SortHierarchically sortClasses: {pointNode}.
	self assert: sorted size = 1.
	self assert: (sorted includes: pointNode)
]

{ #category : #tests }
SortHierarchicallyTests >> testPartialTreeReversed [

	self
		nodes: partialTree reversed
		shouldBe: partialTree
]

{ #category : #tests }
SortHierarchicallyTests >> testPartialTreeUnchanged [

	self
		nodes: partialTree
		shouldBe: partialTree
]

{ #category : #tests }
SortHierarchicallyTests >> testPartialTreeUnsorted [

	self
		nodes: (self sortByNameSize: partialTree)
		shouldBe: partialTree
]

{ #category : #tests }
SortHierarchicallyTests >> testUnrelatedClassesReversed [

	self
		nodes: unrelatedClasses reversed
		shouldBe: unrelatedClasses
]

{ #category : #tests }
SortHierarchicallyTests >> testUnrelatedClassesUnchanged [

	self
		nodes: unrelatedClasses
		shouldBe: unrelatedClasses
]

{ #category : #tests }
SortHierarchicallyTests >> testUnrelatedClassesUnsorted [

	self
		nodes: (self sortByNameSize: unrelatedClasses)
		shouldBe: unrelatedClasses
]
