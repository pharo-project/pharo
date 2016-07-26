This class encodes and decodes data in Base64 format.  This is MIME encoding.  We translate a whole stream at once, taking a Stream as input and giving one as output.  Returns a whole stream for the caller to use.
           0 A            17 R            34 i            51 z
           1 B            18 S            35 j            52 0
           2 C            19 T            36 k            53 1
           3 D            20 U            37 l            54 2
           4 E            21 V            38 m            55 3
           5 F            22 W            39 n            56 4
           6 G            23 X            40 o            57 5
           7 H            24 Y            41 p            58 6
           8 I            25 Z            42 q            59 7
           9 J            26 a            43 r            60 8
          10 K            27 b            44 s            61 9
          11 L            28 c            45 t            62 +
          12 M            29 d            46 u            63 /
          13 N            30 e            47 v
          14 O            31 f            48 w         (pad) =
          15 P            32 g            49 x
          16 Q            33 h            50 y
Outbound: bytes are broken into 6 bit chunks, and the 0-63 value is converted to a character.  3 data bytes go into 4 characters.
Inbound: Characters are translated in to 0-63 values and shifted into 8 bit bytes.

(See: N. Borenstein, Bellcore, N. Freed, Innosoft, Network Working Group, Request for Comments: RFC 1521, September 1993, MIME (Multipurpose Internet Mail Extensions) Part One: Mechanisms for Specifying and Describing the Format of Internet Message Bodies. Sec 6.2)

By Ted Kaehler, based on Tim Olson's Base64Filter.