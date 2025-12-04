@default:
	just --choose

day1:
	uiua run day01/task.ua

day2:
	swipl -qs day02/task.pro -t halt

day3:
	nu day03/task.nu -n 12
