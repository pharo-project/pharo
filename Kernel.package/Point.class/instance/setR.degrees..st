private
setR: rho degrees: degrees 
	| radians |
	radians := degrees asFloat degreesToRadians.
	x := rho asFloat * radians cos.
	y := rho asFloat * radians sin