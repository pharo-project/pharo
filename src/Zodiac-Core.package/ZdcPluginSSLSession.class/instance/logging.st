accessing
logging

	^ ((self primitiveSSL: handle getIntProperty: self constantsSQSSL_PROP_LOGLEVEL) = 0) not