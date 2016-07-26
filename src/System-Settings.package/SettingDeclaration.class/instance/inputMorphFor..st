user interface
inputMorphFor: aContainer
	^ (super inputMorphFor: aContainer)
		ifNotNil: [:im |			
				self whenPrivateChangedSend: #settingValueChanged: to: aContainer model.
				self whenPrivateChangedSend: #innerWidgetChanged to: aContainer.
				im]