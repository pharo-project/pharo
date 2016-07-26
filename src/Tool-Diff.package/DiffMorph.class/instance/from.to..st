instance-creation
from: old to: new
	"Set the old (src) and new (dst) text."

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