Extension { #name : #TextPresenter }

{ #category : #'*SmartSuggestions' }
TextPresenter >> sugsContext [
	^ SugsMessageBrowserContext model: self.
]
