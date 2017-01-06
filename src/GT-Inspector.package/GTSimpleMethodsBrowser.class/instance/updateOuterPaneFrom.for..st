private
updateOuterPaneFrom: text for: method
	"this is a utility method to tell the pane surrounding the browser to update.
	The reason we do that is to support the use case of updating the GTInspector from
	the editor by modifying code directly in the Inspector."
	
	| outerPane newMethod |
	newMethod := method methodClass compile: text text notifying: nil.
	outerPane := text pane browser pane.
	outerPane browser panes do: #update.
	self
		flag:
			'This is a huge hack because of so many reasons:
				- the presentation accesses the state of the outer browser directly
				- the presentation depends on the implementation of the browser 
				It should be solved through transmissions.
				
				But, it works for now and the browser is reasonably updated'.
	^ outerPane
		withAllPanesAndPresentationsDo: [ :child | 
			(child isKindOf: self class)
				ifTrue: [ 
					((child first paneNamed: #classes) port: #selection) value: method methodClass.
					((child first paneNamed: #methods) port: #selection)
						value:
							((child first paneNamed: #methods) presentations first displayedValue ifNotNil: [ :v | v detect: [ :each | each selector = newMethod ] ]) ] ]