private
generateRandomLength: bitLength s: s n: n
	"Answer a random number of bitLength bits generated using the secure hash algorithm."

	| sha out count extraBits v |
	sha := SHA1 new.
	out := 0.
	count := (bitLength // 160).
	extraBits := bitLength - (count * 160).
	0 to: count do: [:k |
		v := sha hashInteger: (s + n + k).
		k = count ifTrue: [
			v := v - ((v >> extraBits) << extraBits)].
		out := out bitOr: (v bitShift: (160 * k))].
	^ out
