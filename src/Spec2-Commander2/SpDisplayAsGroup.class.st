"
I display a command group as a group in the MenuPresenter.
"
Class {
	#name : #SpDisplayAsGroup,
	#superclass : #SpCommandGroupDisplayStrategy,
	#category : #'Spec2-Commander2-Core'
}

{ #category : #displaying }
SpDisplayAsGroup >> display: aCmSpecCommandGroup in: aMenuPresenter do: aBlock [

	aMenuPresenter addGroup: [ :menuGroup |
		aBlock value: menuGroup ]
]
