Class {
	#name : #SpStyleMorphTest,
	#superclass : #TestCase,
	#category : #'Spec2-Adapters-Morphic-Tests'
}

{ #category : #tests }
SpStyleMorphTest >> testTwoClassWithSameName [
	| morphStyle style1 style2 property label |

	style1 := SpStyleSTONReader fromString: '
.application [
	.label [ 
		Font { #name: "Source Sans Pro", #size: 10  } ] ]'.
	
	style2 := SpStyleSTONReader fromString: '
.application [
	.label [ 
		Font { #bold: true } ] ]'.

	label := SpLabelPresenter new buildWithSpec; yourself.
	
	morphStyle := SpMorphStyle fromStylesheet: style1 adapter: label adapter.
	property := morphStyle mergedProperties anyOne.
	
	self assert: morphStyle mergedProperties size equals: 1.
	self assert: property class equals: SpStyleFont.
	self assert: property name equals: 'Source Sans Pro'.
	self deny: property isBold.

	morphStyle := SpMorphStyle fromStylesheet: style1, style2 adapter: label adapter.
	property := morphStyle mergedProperties anyOne.
	
	self assert: morphStyle mergedProperties size equals: 1.
	self assert: property class equals: SpStyleFont.
	self assert: property name equals: 'Source Sans Pro'.
	self assert: property isBold
]
