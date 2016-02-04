b genunimp
b suspend
handle SIGUSR1 pass nostop noprint
define da
	handle SIGALRM ign
	handle SIGUSR1 ign
	handle SIGIO ign
end

define ea
	handle SIGALRM pass
	handle SIGUSR1 pass nostop noprint
	handle SIGIO pass nostop noprint
end
