"
I implement a filtering strategy for processors that takes into account the GTSpotterExtensionSettings. Essentially, I will only allow processors that appear as enabled in the Settings Browser.
"
Class {
	#name : #GTSpotterSettingsProcessorFilter,
	#superclass : #GTSpotterProcessorFilter,
	#category : #'GT-Spotter-Model'
}

{ #category : #testing }
GTSpotterSettingsProcessorFilter >> configurationBlockForProcessorsCreatedBy: aMethod [
	"No need to configure anything."
	
	^ [ :processor | ]
]

{ #category : #testing }
GTSpotterSettingsProcessorFilter >> shouldEnableProcessorsCreateBy: aMethod [
	"Enable everything."
	
	^ (GTSpotterExtensionSettings shouldSpotterIgnoreProcessorDefinedInMethod: aMethod) not
]
