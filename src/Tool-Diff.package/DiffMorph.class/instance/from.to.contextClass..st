instance-creation
from: old to: new contextClass: aClass
	"Set the old (src) and new (dst) text."

	self contextClass: aClass.
	self sourceTextModel setText: old.
	self destTextModel setText: new.
	self
		applyPrettyPrinter;
		calculateDifference;
		calculateJoinMappings;
		calibrateScrollbar;
		applyHighlights;
		applyJoin;
		applyMap