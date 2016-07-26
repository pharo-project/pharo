morph creation
windowCollapseBoxLayoutFor: aWindow

	^ LayoutFrame new 
		leftFraction: 1; 
		leftOffset: aWindow boxExtent x negated;
		yourself