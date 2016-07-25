initialization
dynamicButtonServices
	"Answer services for buttons that may come and go in the button pane, depending on selection"

	^ reference isDirectory 
		ifTrue: [#()]
		ifFalse:
			[ | toReject |
				toReject := self buttonSelectorsToSuppress.
				(self itemsForFile: reference) reject:
					[:svc | toReject includes: svc selector]]