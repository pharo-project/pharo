Extension { #name : #PluggableTextMorph }

{ #category : #'*NodeNavigation' }
PluggableTextMorph >> standOutHorizontalScope [

	self handleEdit: [NNavNavigation new navigate: textMorph editor direction: NNavDirectionSibling ] 
]

{ #category : #'*NodeNavigation' }
PluggableTextMorph >> standOutIntoFirstScope [
	self handleEdit: [NNavNavigation new navigate: textMorph editor direction: NNavDirectionFirstChild ]
]

{ #category : #'*NodeNavigation' }
PluggableTextMorph >> standOutIntoScope [
	self handleEdit: [NNavNavigation new navigate: textMorph editor direction:  NNavDirectionChild]
]

{ #category : #'*NodeNavigation' }
PluggableTextMorph >> standOutOverScope [
	self handleEdit: [NNavNavigation new navigate: textMorph editor direction:  NNavDirectionParent] 
]

{ #category : #'*NodeNavigation' }
PluggableTextMorph >> standOutPreviousChild [
	self handleEdit: [
		NNavNavigation new 
			navigate: textMorph editor 
			direction: NNavDirectionPreviousBrother 
	]
]

{ #category : #'*NodeNavigation' }
PluggableTextMorph >> standOutPreviousSelection [
	self handleEdit: [
		NNavNavigation new 
			navigate: textMorph editor 
			direction: NNavDirectionPreviousSelection  
	]
]
