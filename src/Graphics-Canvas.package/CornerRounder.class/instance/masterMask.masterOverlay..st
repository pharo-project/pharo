all
masterMask: maskForm masterOverlay: overlayForm

	cornerMasks := #(none left pi right) collect:
		[:dir | (maskForm rotateBy: dir centerAt: 0@0) offset: 0@0].
	cornerOverlays := #(none left pi right) collect:
		[:dir | (overlayForm rotateBy: dir centerAt: 0@0) offset: 0@0].
