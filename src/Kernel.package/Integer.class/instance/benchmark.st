benchmarks
benchmark  "Handy bytecode-heavy benchmark"
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