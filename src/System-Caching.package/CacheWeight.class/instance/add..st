accessing
add: value
	| weight |
	weight := compute cull: value.
	total := total + weight