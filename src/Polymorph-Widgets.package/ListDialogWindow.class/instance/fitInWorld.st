geometry
fitInWorld
	"Unlike Morph>>#fitInWorld, avoid occlusion by docking toolbars"
	| boundsFitIntoWorld |
	boundsFitIntoWorld := self bounds translatedAndSquishedToBeWithin: self world clearArea.
	self bounds: boundsFitIntoWorld