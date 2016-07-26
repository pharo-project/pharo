change reporting
invalidRect: damageRect from: aMorph
	"Clip damage reports to my bounds, since drawing is clipped to my bounds."

	worldState recordDamagedRect: (damageRect intersect: self bounds ifNone: [ ^ self ])