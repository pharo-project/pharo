popup - opening
openAt: aPoint
	"Open me in the world at a given position provided in global coordinates.
	Return a morphic foundation brick that is used to wrap a popup"
	<return: #GLMFoundationBrick>
	| aFoundationBrick |

	aFoundationBrick := self asBrickedMorph
		vShrinkWrap;
		hShrinkWrap;
		position: aPoint;
		openInWorld.

	self onOpened.
	
	^ aFoundationBrick