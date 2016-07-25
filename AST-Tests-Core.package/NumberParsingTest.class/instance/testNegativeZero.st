tests - Integer
testNegativeZero
      "This test ensure that -0.0 will produce a negativeZero"

      | negativeZero |
      negativeZero := Number readFrom: '-0.0' readStream.

      "If it is a negative zero, it must behave like a negative zero... IEEE 754 tells how it should behave"
      self deny: (negativeZero at: 1) = 0 description: 'In IEEE 754, a negative zero has its sign bit set to 1'..
      self assert: negativeZero = 0 description: 'In IEEE 754, a negative zero cannot be distinguished from zero'.