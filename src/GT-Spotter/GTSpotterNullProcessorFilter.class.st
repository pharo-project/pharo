"
I implement a filtering strategy for processors that always returns true. Use me by default instead of adding null checks to determnine if a filter is present.
"
Class {
	#name : #GTSpotterNullProcessorFilter,
	#superclass : #GTSpotterProcessorFilter,
	#category : #'GT-Spotter-Model'
}

{ #category : #testing }
GTSpotterNullProcessorFilter >> configurationBlockForProcessorsCreatedBy: aMethod [
	"No need to configure anything."
	
	^ [ :processor | ]
]

{ #category : #testing }
GTSpotterNullProcessorFilter >> shouldEnableProcessorsCreateBy: aMethod [
	"Enable everything."
	
	^ true
]
