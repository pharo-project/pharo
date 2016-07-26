tests - mathematical functions
testCopySignTo

      "Set up"
      | negatives negz positives strictNegatives strictPositives zero |
      strictPositives := {2. 2.5. Float infinity}.
      strictNegatives := {-3. -3.25. Float infinity negated}.
      zero := 0.0.
      negz := Float negativeZero.
      positives := strictPositives copyWith: zero.
      negatives := strictNegatives copyWith: negz.

      "Test the copy sign functions"
      positives do: [:aPositiveSign |
              positives do: [:aPositive |
                      self assert: (aPositiveSign copySignTo: aPositive) = aPositive].
              negatives do: [:aNegative |
                      self assert: (aPositiveSign copySignTo: aNegative) = aNegative negated].
              (aPositiveSign copySignTo: zero) sign = 0.
              (aPositiveSign copySignTo: negz) sign = 0].

      negatives do: [:aNegativeSign |
              positives do: [:aPositive |
                      self assert: (aNegativeSign copySignTo: aPositive) = aPositive negated].
              negatives do: [:aNegative |
                      self assert: (aNegativeSign copySignTo: aNegative) = aNegative].
              (aNegativeSign copySignTo: zero) sign = -1.
              (aNegativeSign copySignTo: negz) sign = -1].