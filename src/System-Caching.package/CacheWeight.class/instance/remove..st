accessing
remove: value
	| weight |
	weight := compute cull: value.
	total := total - weight