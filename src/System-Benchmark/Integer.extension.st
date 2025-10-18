Extension { #name : 'Integer' }

{ #category : '*System-Benchmark' }
Integer >> benchFib [
	"Handy send-heavy benchmark"
	"(result // seconds to run) = approx calls per second"
	" | r t |
	  t := Time millisecondsToRun: [r := 26 benchFib].
	  (r * 1000) // t"
	"138000 on a Mac 8100/100"
	^ self < 2
		ifTrue: [1]
		ifFalse: [(self-1) benchFib + (self-2) benchFib + 1]
]

{ #category : '*System-Benchmark' }
Integer >> benchmark [  "Handy bytecode-heavy benchmark"
	"(500000 // time to run) = approx bytecodes per second"
	"5000000 // (Time millisecondsToRun: [10 benchmark]) * 1000"
	"3059000 on a Mac 8100/100"
    | size flags prime k count |
    size := 8190.
    1 to: self do:
        [:iter |
        count := 0.
        flags := (Array new: size) atAllPut: true.
        1 to: size do:
            [:i | (flags at: i) ifTrue:
                [prime := i+1.
                k := i + prime.
                [k <= size] whileTrue:
                    [flags at: k put: false.
                    k := k + prime].
                count := count + 1]]].
    ^ count
]

{ #category : '*System-Benchmark' }
Integer >> tinyBenchmarks [

	| t1 t2 r n1 n2 |
	n1 := 1.
	[
	t1 := [ n1 benchmark ] millisecondsToRun.
	t1 < 1000 ] whileTrue: [ n1 := n1 * 2 ]. "Note: #benchmark's runtime is about O(n)"

	n2 := 28.
	[
	t2 := [ r := n2 benchFib ] millisecondsToRun.
	t2 < 1000 ] whileTrue: [ n2 := n2 + 1 ]. "Note: #benchFib's runtime is about O(n^2)."

	^ (n1 * 500000 * 1000 // t1) printString , ' bytecodes/sec; '
	  , (r * 1000 // t2) printString , ' sends/sec'
]
