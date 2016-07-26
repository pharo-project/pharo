tests - mathematical functions
testSign

      "Set up"
      | negatives negz positives strictNegatives strictPositives zero |
      strictPositives := {2. 2.5. Float infinity}.
      strictNegatives := {-3. -3.25. Float infinity negated}.
      zero := 0.0.
      negz := Float negativeZero.
      positives := strictPositives copyWith: zero.
      negatives := strictNegatives copyWith: negz.

      "The sign of non zeros"
      strictPositives do: [:aPositive | self assert: aPositive sign = 1].
      strictNegatives do: [:aNegative | self assert: aNegative sign = -1].

      "The sign of zeros"
      self assert: zero sign = 0.
      self assert: negz sign = -1. "remark though that negz >= 0.0, and is thus considered positive... Weird"

      "Test the copy sign functions"
      positives do: [:aPositiveSign |
              positives do: [:aPositive |
                      self assert: (aPositive sign: aPositiveSign) = aPositive].
              negatives do: [:aNegative |
                      self assert: (aNegative sign: aPositiveSign) = aNegative negated].
              (zero sign: aPositiveSign) sign = 0.
              (negz sign: aPositiveSign) sign = 0].

      negatives do: [:aNegativeSign |
              positives do: [:aPositive |
                      self assert: (aPositive sign: aNegativeSign) = aPositive negated].
              negatives do: [:aNegative |
                      self assert: (aNegative sign: aNegativeSign) = aNegative].
              (zero sign: aNegativeSign) sign = -1.
              (negz sign: aNegativeSign) sign = -1].