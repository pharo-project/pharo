spec protocol
updateAccordingTo: aFilter

	self widgetDo: [ :w |
		w contents do: [ :n | n nodeModel updateAccordingTo: aFilter ].
		w contents removeAllSuchThat: [ :n | (aFilter keepTreeNode: n nodeModel) not  and: [ n nodeModel isEmpty ] ] ]