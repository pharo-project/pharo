private
updateBalloonText

	target ifNotNil: [ self balloonText: (target perform: balloonTextSelector) ]