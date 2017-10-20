comparing
hash
    ^ ((String stringHash: projectName initialHash: 0) bitXor: configurationProjectSpec metacelloRegistrationHash)
        bitXor: baselineProjectSpec metacelloRegistrationHash