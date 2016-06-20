" Symbol checker 0.58
" ====================================================
" '<CR>' : Display hexadecimal and binary values
"          Check current kernel configuration (.config)
"          Jump to the definition of the keyword (tag)
" '\'   : C-style calculator
"         ex) 0x1234 & ((1 << 12) -1)
" =================== GIT features ===================
" 'gb'    : blame (support visual block)
" 'gl'    : logs (support visual block)
" '<CR>'  : select log, show diff
" 'd'     : show diff
" 's'     : show all changeset
" 'f'     : show a file
" '['     : find previous
" ']'     : find next
" 'q'     : close window
" ===================== OPTIONS =====================
" let g:git_window = [vert, hori(default), none]
" let g:git_resize = [vert, hori, both(default), none]
" let g:git_scroll = [top, center(default), none]
" let g:git_merges = [0, 1(default)]

let s:nibble = ["0000", "0001", "0010", "0011", "0100", "0101", "0110", "0111", "1000", "1001", "1010", "1011", "1100", "1101", "1110", "1111"]

if exists("*or")
	let s:bitfunc = ["invert", "and", "xor", "or"]
else
	let s:bitfunc = ["Invert", "And", "Xor", "Or"]
endif

if !exists('g:git_window') | let g:git_window = "hori" | endif
if !exists('g:git_resize') | let g:git_resize = "both" | endif
if !exists('g:git_scroll') | let g:git_scroll = "center" | endif
if !exists('g:git_merges') | let g:git_merges = 1 | endif

fu! Bitwise(var1, var2, one, two)
	let b1 = Num2Bin(a:var1)
	let b2 = Num2Bin(a:var2)
	let p1 = strlen(b1) - 1
	let p2 = strlen(b2) - 1
	let result = ""
        while p1 >= 0 || p2 >= 0
		let bit = strpart(b1, p1, 1) + strpart(b2, p2, 1)
		if bit == 0
			let result = "0" . result
		elseif bit == 1
			let result = a:one . result
		elseif bit == 2
			let result = a:two . result
		endif
		let p1 -= 1
		let p2 -= 1
	endw
	return Bin2Num(" " . result . " ")
endfu

fu! Invert(var)
	return Bitwise(a:var, 0xffffffff, 1, 0)
endfu

fu! And(var1, var2)
	return Bitwise(a:var1, a:var2, 0, 1)
endfu

fu! Xor(var1, var2)
	return Bitwise(a:var1, a:var2, 1, 0)
endfu

fu! Or(var1, var2)
	return Bitwise(a:var1, a:var2, 1, 1)
endfu

fu! Bin2Num(bin)
	let i = 0
        let num = 0
	let len = strlen(a:bin)
        while i < len
		let c = strpart(a:bin,i,1)
		let i += 1
		if c !~ '[01]' | continue | endif
                let num = num * 2 + c
        endw
        return printf("%u",num)
endfu

fu! Num2Bin(var)
	let num = printf("%x", a:var)
	let bin = ""
	let pos = 0
	let len = strlen(num)
	while pos < len
		let bin = s:nibble[str2nr(num[-1:],16)] . bin
		let num = num[0:-2]
		let pos += 1
	endw
	return bin
endfu

fu! Commas(var, nr)
	let result = ""
	let str = a:var
	let i = strlen(str) - a:nr
	let result = strpart(str,i)
	while i > 0
		let i -= a:nr
		let result = strpart(str,i,a:nr) . "," . result
	endw
	return result
endfu

fu! BinFmt(var)
	let b = Num2Bin(a:var)
	return Commas(b,4) . " (" . strlen(b) . ")"
endfu

fu! Shl(var,n)
	let s = Num2Bin(a:var)
	for i in range(a:n)
		let s .= "0"
	endfo
	return Bin2Num(s)
endfu

fu! Shr(var,n)
	let s = Num2Bin(a:var)
	for i in range(a:n)
		let s = s[0:-2]
	endfo
	return Bin2Num(s)
endfu

fu! NumFmt(sym)
	let unit = ['B', 'K', 'M', 'G', 'T', 'P', 'E', 'Z']
	let cnt = 0
	let sym = printf("%u",a:sym)
	let s = Num2Bin(sym)

	while (s[-10:] =~ '0\{10}') && strlen(s) > 0
		let s = s[0:-11]
		let cnt += 1
	endw
	if cnt == 0
		let units = ""
	else
		let units = printf("(%u%s) ", Bin2Num(s), unit[cnt])
	endif
	return printf("D:%s %sH:0x%x B:%s", Commas(sym,3), units, sym, BinFmt(sym))
endfu

fu! CheckSymbol(var)
	let sym = a:var
	if sym =~# '^CONFIG_'
		let config = findfile(".config", ".;")
		if config != ""
			let hit = 0
			for line in readfile(config, '')
				if line =~# sym . '[= ]'
					echo line
					let hit += 1
					break
				endif
			endfo
			if hit == 0 | echo "# " . sym . " not found" | endif
		else
			echo "config file not found"
		endif
	elseif sym =~? '^\(#\|=\)\?\([0-9]\+\|0x[0-9a-f]\+\)$'
		if sym =~ '^\(=\|#\)' | let sym = sym[1:] | endif
		echo NumFmt(sym)
	else
		if &l:cscopetag == 0
			let v:errmsg = ""
			silent! exec "ta " . sym
			if v:errmsg == ""
				:0tn
			endif
		else
			exec "ta " . sym
		endif
	endif
endfu

fu! SplitExp(exp)
	let n = 0
	let spc = 0
	let max = strlen(a:exp)
	let sign = 0
	let last = 0
	let token = []
	while n < max
		if strpart(a:exp,n,1) == " "
			let sign = 1
			let spc = 1
		elseif strpart(a:exp,n,1) =~ '[+\-*/%()^&|~]'
			let sign = 1
		elseif strpart(a:exp,n,2) =~ '\(0b\|<<\|>>\)'
			let sign = 2
		endif

		if sign > 0
			if last != n
				call add(token, strpart(a:exp,last,n-last))
			endif
			if spc == 0
				call add(token, strpart(a:exp,n,sign))
			else
				let spc = 0
			endif
			let n += sign
			let last = n
			let sign = 0
		else
			let n += 1
		endif
	endw
	if last != n
		call add(token, strpart(a:exp, last, n-last))
	endif
	return token
endfu

fu! MatchParen(str, pos)
	let pos = a:pos
	if a:str[pos] == ')'
		let depth = 1
		let n = -1
	elseif a:str[pos] == '('
		let depth = 1
		let n = 1
	endif
	while depth > 0
		let pos += n
		if a:str[pos] == '('
			let depth += n
		elseif a:str[pos] == ')'
			let depth -= n
		endif
	endw
	return pos

endfu

fu! Exp2Func(tok, pat, func, argc)
	let str = a:tok
	if len(a:pat) <= 1
		let regex = a:pat[0]
	else
		let regex = '\(' . substitute(join(a:pat), ' ', '\\|', 'g') . '\)'
	endif

	while match(str, regex) != -1

		let pos = match(str, regex)
		for i in range(len(a:pat))
			if str[pos] =~ '^' . a:pat[i] . '$'
				let func = a:func[i]
				let argc = a:argc[i]
				break
			endif
		endfor

		if a:argc[i] >= 2
			let beg = pos - 1
			if str[beg] == ')'
				let beg = MatchParen(str, beg)
			endif
			if str[beg-1] =~ '\w' && beg > 0
				let beg -= 1
			endif
			let str[pos] = ','
			call insert(str, func, beg)
			let beg += 1
			let pos += 1
		else
			let beg = pos + 1
			let str[pos] = func
		endif

		let end = pos + 1
		if str[end] =~ '\w' && end + 1 < len(str)
			let end += 1
		endif
		if str[end] == '('
			let end = MatchParen(str, end)
		endif
		if end+1 < len(str)
			call insert(str, ')', end)
		else
			call add(str, ')')
		endif

		call insert(str, '(', beg)
	endw
	return str
endfu

fu! Calc(var)
	if a:var != ""
		let exp = SplitExp(a:var)
		let exp = Exp2Func(exp, ['0b'], ['Bin2Num'], [1])
		let i = 0
		while i < len(exp) - 3
			if exp[i] == "Bin2Num" && exp[i+1] == "("
				if exp[i+2] =~ '^0\+'
					let exp[i+2] = substitute(exp[i+2],'^0\+','','g')
				endif
				call insert(exp,'"',i+3)
				call insert(exp,'"',i+2)
				let i += 2
			endif
			let i += 1
		endw
		let exp = Exp2Func(exp, ['\~'], [s:bitfunc[0]], [1])
		let exp = Exp2Func(exp, ['<<', '>>'], ['Shl', 'Shr'], [2, 2])
		let exp = Exp2Func(exp, ['&'], [s:bitfunc[1]], [2])
		let exp = Exp2Func(exp, ['\^'], [s:bitfunc[2]], [2])
		let exp = Exp2Func(exp, ['|'], [s:bitfunc[3]], [2])
		echohl MoreMsg
		echo " = " . NumFmt(eval(join(exp)))
		echohl None
	endif
endfu
fu! GitResize()
	if g:git_resize == "none"

	elseif g:git_resize == "vert"
		vert res
	elseif g:git_resize == "hori"
		res
	else
		vert res | res
	endif
endfu

fu! GitNewWindow(title)
	if exists('b:file')
		let l:path = b:path
		let l:file = b:file
	else
		if empty(expand("%:t"))
			return -1
		endif
		let l:path = fnameescape(expand("%:p:h"))
		let l:file = fnameescape(expand("%:t"))
	endif

	if g:git_window == "none"
		enew
	elseif g:git_window == "vert"
		vert bot new
	else
		bot new
	endif
	call GitResize()

	setl buftype=nowrite bufhidden=wipe nobuflisted noswapfile nowrap
	let b:path = l:path
	let b:file = l:file
	setl incsearch

	let l:bufproto = a:title . ":" . b:file
	let l:buf= l:bufproto
	let l:cnt = 0

	while bufwinnr(l:buf) > 0
		let l:cnt += 1
		let l:buf = l:bufproto . "-" . l:cnt
	endwh
	exec "file " . l:buf
	return 0
endfu

fu! GitExec(cmd)
	if has("win32")
		lcd `=b:path`
		let l:cmd_cd = ""
	else
		let l:cmd_cd = "cd " . b:path . ";"
	endif

	silent! execute "0read ! " . l:cmd_cd . a:cmd
	setl noma | 1
endfu

fu! GitBlame() range
	let l:line = line(".")
	if GitNewWindow("git-blame") < 0
		echo "File not found"
		return
	endif

	if a:firstline == a:lastline
		call GitExec('git blame ' . b:file)
		execute l:line
	else
		call GitExec('git blame -L ' . a:firstline .','. a:lastline .' '. b:file)
	endif
	setl cursorline
	noremap <silent> <buffer> ] :call FindHash(1)<CR>
	noremap <silent> <buffer> [ :call FindHash(-1)<CR>
	noremap <silent> <buffer> f 0:call GitFile(GetHash())<CR>
	noremap <silent> <buffer> s 0:call GitShow(GetHash())<CR>
	noremap <silent> <buffer> d 0:call GitShow(GetHash(),1)<CR>
	noremap <silent> <buffer> <CR> 0:call GitLog(GetHash())<CR>
	noremap <silent> <buffer> q :close<CR>:call GitResize()<CR>
endfu

fu! ValidHash(hash)
	if empty("a:hash")
		echo "Hash not found"
		return -1
	elseif a:hash =~? '[^0-9a-f]' || strlen(a:hash) > 40 || strlen(a:hash) < 4
		echo "Invalid hash : " . a:hash
		return -2
	endif
	return 0
endfu

fu! GetHash()
	let l:hash = expand("<cword>")
	if l:hash == "commit"
		let l:pos=getpos('.')
		norm! w
		let l:hash = expand("<cword>")
		call setpos('.', l:pos)
	endif
	return l:hash
endfu

fu! FindHash(step)
	norm! g0
	let l:hash = expand("<cword>")
	let l:line = line(".") + a:step
	execute l:line
	while l:hash == expand("<cword>") && l:line > 0 && l:line <= line("$")
		let l:line += a:step
		execute l:line
	endwh
endfu

fu! GitPrev(str)
	call search(a:str,"Wb")
	if line(".") <= line("w0")
		if g:git_scroll == "top"
			norm! zt
		elseif g:git_scroll == "center"
			norm! z.
		endif
	endif
endfu

fu! GitNext(str)
	call search(a:str,"W")
	if line("w$") <= line(".")
		if g:git_scroll == "top"
			norm! zt
		elseif g:git_scroll == "center"
			norm! z.
		endif
	endif
endfu

fu! GitLog(hash)
	if ValidHash(a:hash) < 0  | return | endif
	if GitNewWindow("git-log") < 0 | return | endif
	call GitExec('git show -s ' . a:hash)
	setl syntax=git
	noremap <silent> <buffer> [ :call GitPrev("^commit ")<CR>
	noremap <silent> <buffer> ] :call GitNext("^commit ")<CR>
	noremap <silent> <buffer> f 0:call GitFile(GetHash())<CR>
	noremap <silent> <buffer> s 0:call GitShow(GetHash())<CR>
	noremap <silent> <buffer> d 0:call GitShow(GetHash(),1)<CR>
	noremap <silent> <buffer> <CR> 0:call GitShow(GetHash(),1)<CR>
	noremap <silent> <buffer> q :close<CR>:call GitResize()<CR>
endfu

fu! GitFullLog() range
	if exists('g:git_merges') && g:git_merges == 0
		let l:opts = ' --no-merges '
	else
		let l:opts = ''
	endif
	if GitNewWindow("git-log") < 0 | return | endif
	if a:firstline == a:lastline
		call GitExec('git log ' . l:opts . b:file)
	else
		call GitExec('git log -L ' . a:firstline .','. a:lastline .':'. b:file . ' ' . l:opts)
	endif

	setl syntax=git
	noremap <silent> <buffer> [ :call GitPrev("^commit ")<CR>
	noremap <silent> <buffer> ] :call GitNext("^commit ")<CR>
	noremap <silent> <buffer> f 0:call GitFile(GetHash())<CR>
	noremap <silent> <buffer> s 0:call GitShow(GetHash())<CR>
	noremap <silent> <buffer> d 0:call GitShow(GetHash(),1)<CR>
	noremap <silent> <buffer> <CR> 0:call GitShow(GetHash(),1)<CR>
	noremap <silent> <buffer> q :close<CR>:call GitResize()<CR>
endfu

fu! GitShow(hash, ...)
	if ValidHash(a:hash) < 0  | return | endif
	if !exists('g:git_merges') || g:git_merges != 0
		let l:opts = ' -m '
	else
		let l:opts = ''
	endif
	if GitNewWindow("git-show") < 0 | return | endif

	if a:0 > 0 | let l:opts .= b:file | endif

	call GitExec('git show --format=oneline -p ' . a:hash . ' ' . l:opts)
	setl syntax=diff
	noremap <silent> <buffer> [ :call GitPrev("^diff ")<CR>
	noremap <silent> <buffer> ] :call GitNext("^diff ")<CR>
	noremap <silent> <buffer> q :close<CR>:call GitResize()<CR>
endfu

fu! GitFile(hash)
	if ValidHash(a:hash) < 0  | return | endif
	if GitNewWindow(a:hash) < 0 | return | endif

	call GitExec('git show -p ' . a:hash . ':./' . b:file)
	noremap <silent> <buffer> q :close<CR>:call GitResize()<CR>
endfu

nnoremap <silent> \ :call Calc(input("Calculate: "))<CR>
nnoremap <silent> <CR> :call CheckSymbol(expand("<cword>"))<CR>
noremap <silent> gb :call GitBlame()<CR>
noremap <silent> gl :call GitFullLog()<CR>
