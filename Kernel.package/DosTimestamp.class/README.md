DOS stores timestamps, in local time, as 32 bit integers with the following format:
	32 bits (low to high):
		Low 16 bits:
			Bits 0-4: seconds / 2
	 		Bits 5-10: minutes 0-59
			Bits 11-15: hours 0-23
		High 16 bits:
			16-20: day of month 1-31
	 		21-24: month 1-12
			25-31: year offset from 1980 (e.g. 1981 -> 1)

References (with visual aids): http://blogs.msdn.com/b/oldnewthing/archive/2003/09/05/54806.aspx & http://mindprod.com/jgloss/zip.html