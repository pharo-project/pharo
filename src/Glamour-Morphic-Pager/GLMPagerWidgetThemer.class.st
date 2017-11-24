Class {
	#name : #GLMPagerWidgetThemer,
	#superclass : #Object,
	#category : #'Glamour-Morphic-Pager-Theme'
}

{ #category : #'as yet unclassified' }
GLMPagerWidgetThemer >> defaultScrollButtonStyleFor: aButton [

	aButton checkedIcon: GLMUIThemeExtraIcons glamorousGrayCircleEmpty.
	aButton checkedFocusedIcon: GLMUIThemeExtraIcons glamorousGrayCircle.
	
	aButton normalImage: GLMUIThemeExtraIcons glamorousGrayCircleEmpty.
	aButton selectedImage: GLMUIThemeExtraIcons glamorousGrayCircle.
	aButton checkedImage: aButton checkedIcon.
]

{ #category : #'as yet unclassified' }
GLMPagerWidgetThemer >> previewPaneStyleFor: aMorph [

	aMorph borderColor: (Color lightGray)
]

{ #category : #'as yet unclassified' }
GLMPagerWidgetThemer >> selectedPaneStyleFor: aMorph [

	aMorph borderColor: (Color transparent)
]
