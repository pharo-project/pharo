class initialization
initialize
	"SecureHashAlgorithm initialize"
	"For the curious, here's where these constants come from:
	  #(2 3 5 10) collect: [:x | ((x sqrt / 4.0) * (2.0 raisedTo: 32)) truncated hex]"
	K1 := ThirtyTwoBitRegister new load: 1518500249.
	K2 := ThirtyTwoBitRegister new load: 1859775393.
	K3 := ThirtyTwoBitRegister new load: 2400959708.
	K4 := ThirtyTwoBitRegister new load: 3395469782