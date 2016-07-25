accessing
rotationDegrees: degrees
	self adjustAfter:[self angle: degrees degreesToRadians negated]