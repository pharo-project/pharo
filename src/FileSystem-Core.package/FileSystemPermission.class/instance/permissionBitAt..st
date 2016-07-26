accessing
permissionBitAt: bitIndex
	^ (posixPermission bitAt: bitIndex) == 1