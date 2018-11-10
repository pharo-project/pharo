"
I handle the case that all the items in the trees used in iceberg
are wrapped in FTBasicItem. 

"
Class {
	#name : #IceTipTreeSearchStrategy,
	#superclass : #FTVisibleItemsStrategy,
	#category : #'Iceberg-TipUI-View-DataSource'
}

{ #category : #accessing }
IceTipTreeSearchStrategy >> isMatching: anItem [
	"I have to unwrap the FTBasicItem before comparing in the search"
	^ (dataSource toString: anItem data) beginsWith: pattern
]
