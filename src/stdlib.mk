~~ TODO RegExp portion of stdlib which will include commonly used RegExps.

regexp LOWER;
regexp UPPER;
regexp NUMERIC;
regexp ALPHA;
regexp ALPHANUMERIC
regexp DIGITS;
regexp ASCII;
LOWER = lit 'a' | lit 'b' | lit 'c' | lit 'd' | lit 'e' | lit 'f' | lit 'g' | lit 'h' | lit 'i' | lit 'j' | lit 'k' | lit 'l' | lit 'm' | lit 'n' | lit 'o' | lit 'p' | lit 'q' | lit 'r' | lit 's' | lit 't' | lit 'u' | lit 'v' | lit 'w' | lit 'x' | lit 'y' | lit 'z';
UPPER = lit 'A' | lit 'B' | lit 'C' | lit 'D' | lit 'E' | lit 'F' | lit 'G' | lit 'H' | lit 'I' | lit 'J' | lit 'K' | lit 'L' | lit 'M' | lit 'N' | lit 'O' | lit 'P' | lit 'Q' | lit 'R' | lit 'S' | lit 'T' | lit 'U' | lit 'V' | lit 'W' | lit 'X' | lit 'Y' | lit 'Z';
NUMERICS = lit '0' | lit '1' | lit '2' | lit '3' | lit '4' | lit '5' | lit '6' | lit '7' | lit '8' | lit '9';
ALPHA = alphaLower | alphaUpper;
ALPHANUMERIC = alpha | numeric;
DIGITS = numeric **;
ASCII = lit ' ' | lit '!' | lit '"' | lit '#' | lit '$' | lit '%' | lit '&' | lit '\'' | lit '(' | lit ')' | lit '*' | lit '+' | lit ',' | lit '-' | lit '.' | lit '/' | digits | lit ':' | lit ';' | lit '<' | lit '=' | lit '>' | lit '?' | lit '@' | alphaUpper | lit '[' | lit '\' | lit ']' | lit '^' | lit '_' | lit '`' | alphaLower | lit '{' | lit '|' | lit '}' | lit '~' | lit 'DEL' 


isNumeric : (s : string) -> bool {
  return (digits matches s);
}
isAscii : (s : string) -> bool {
	return (ascii matches s);
}
