def list-index-of [item] {
    $in | enumerate | where { |row| $row.item == $item } | first | get index
}

export def main [
	--file (-f): string = "data/task.example"
	-n: int = 2
] {
	open $file 
	| lines 
	| split chars 
	| each {|line| 
		let bank = $line | into int 

		0..<$n | reduce --fold {start: 0 acc: 0} {|digits, acc|
			let rest = $bank | skip $acc.start
			let available = $rest | take (($in | length) - ($n - $digits - 1))

			let m = $available | math max
			let start = $acc.start + ($rest | list-index-of $m) + 1
			{start: $start acc: ($acc.acc * 10 + $m)}
		} | select acc
	} | math sum
}
