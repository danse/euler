
import dayjs from "dayjs"
import * as fs from 'node:fs/promises'



export function pandigitalMultiples(m1:number, m2:number) {
  function isPandigital(s:string) {
    var l = s.split('')
    l.sort()
    return listEquals(l, ['1', '2', '3', '4', '5', '6', '7', '8', '9'])
  }
  function multiple(n:number, l:number[]) {
    const p = l.map(x => String(x*n))
    return p.reduce((a, b) => a.concat(b), '')
  }
  var multiples = []
  for (var i=1; i<m1; i++) {
    for (var j=1; j<m2; j++) {
      const l = range(1, j)
      const s = multiple(i, l)
      if (isPandigital(s)) {
        multiples.push(Number(s))
      }
    }
  }
  return maximum(multiples)
}

function range(n1:number, n2:number) {
  var r = []
  for (var i=n1; i<n2; i++) {
    r.push(i)
  }
  return r
}

export function truncatablePrimes() {
  function predicate(n:number) {
    function l(s:string) {
      if (s === '') {
        return true
      } else {
        const s2 = s.slice(1)
        if (s2 === '') {
          return true
        }
        if (isPrime(Number(s2))) {
          return l(s2)
        } else {
          return false
        }
      }
    }
    function r(s:string) {
      if (s === '') {
        return true
      } else {
        const s2 = s.slice(-s.length, -1)
        if (s2 === '') {
          return true
        }
        if (isPrime(Number(s2))) {
          return r(s2)
        } else {
          return false
        }
      }
    }
    const s = String(n)
    return isPrime(n) && l(s) && r(s)
  }
  var s = 0
  var c = 0
  for (var i=10; c<11; i++) {
    if (predicate(i)) {
      s += i
      c++
    }
  }
  return s
}

export function doubleBasePalindromes() {
  function isBaseTwoPalindrome(n:number) {
    return isPalindrome(n.toString(2))
  }
  function predicate(n:number) {
    const b10 = isPalindrome(String(n))
    const b2 = isBaseTwoPalindrome(n)
    return b10 && b2
  }
  var s = 0
  for (var i=1; i<1000000; i++) {
    if (predicate(i)) {
      s += i
    }
  }
  return s
}

export function circularPrimes(m:number) {
  function cycles(n:number) {
    function c(s:string) {
      const [a, b] = splitAt(s, 1)
      return b.concat(a)
    }
    var t = String(n)
    var y = []
    for (var i=0; i<t.length; i++) {
      t = c(t)
      y.push(t)
    }
    return y
  }
  function predicate(n:number) {
    const c = cycles(n)
    for (var e of c) {
      if (!isPrime(Number(e))) {
        return false
      }
    }
    return true
  }
  var p = []
  for (var i=2; i<m; i++) {
    if (predicate(i)) {
      p.push(i)
    }
  }
  return p.length
}

export function digitFactorials(m:number) {
  function isDigitFactorial(n:number) {
    function f(t:string) {
      return Number(factorial(Number(t)))
    }
    const s = sum(String(n).split('').map(f))
    return s === n
  }
  var r = 0
  for (var i=10; i<=m; i++) {
    if (isDigitFactorial(i)) {
      r += i
    }
  }
  return r
}

export function digitCancellingFractions() {
  function isDigitCancelling(n:number, d:number) {
    function f(b:boolean, g:number) {
      const n2 = Number(String(n).replace(String(g), ''))
      const d2 = Number(String(d).replace(String(g), ''))
      const l = String(n2).length < String(n).length
      const e = n2/d2 == n/d
      return b || (e && l)
    }
    return [1, 2, 3, 4, 5, 6, 7, 8, 9].reduce(f, false)
  }
  var c = []
  for (var n=10; n<=98; n++) {
    for (var d=n+1; d<=99; d++) {
      if (isDigitCancelling(n, d)) {
        c.push([n, d])
      }
    }
  }
  return c
}

export function pandigitalProducts() {
  const pe = perm(['1', '2', '3', '4', '5', '6', '7', '8', '9'])
  var products = []
  for (var p of pe) {
    for (var i=1; i<=8; i++) {
      for (var j=1; j<=8-i; j++) {
        const [p1, p2] = splitAt(p, i)
        const [p21, p22] = splitAt(p2, j)
        const a = Number(p1)
        const b = Number(p21)
        const c = Number(p22)
        if (a * b === c) {
          products.push(c)
        }
      }
    }
  }
  products.sort()
  const u = uniq(products)
  return sum(u)
}

export function splitAt(s:string, n:number) {
  const s1 = s.slice(0, n)
  const s2 = s.slice(n)
  return [s1, s2]
}

export function coinSums(m:number) {
  function f(c:number[], t:number) {
    const n = c.pop()
    if (n === undefined) { // empty list
      if (t === 0) {
        return 1
      } else {
        return 0
      }
    }
    var count = 0
    for (var i = 0; i <= t / n; i++) {
      const value = i * n
      count += f(structuredClone(c), t - value)
    }
    return count
  }
  return f([1, 2, 5, 10, 20, 50, 100, 200], m)
}

export function binaryMask(l:number): boolean[][] {
  if (l===1) {
    return [[false], [true]]
  }
  const r = binaryMask(l-1)
  var f = []
  var t = []
  for (var e of r) {
    f.push([false].concat(e))
    t.push([true].concat(e))
  }
  return f.concat(t)
}

export function fifthPowers(m:number) {
  function predicate(n:number) {
    let g = function(value:string) { return Number(value) ** 5 }
    let p = sum(String(n).split('').map(g))
    return p === n
  }
  let p = []
  for (var i=2; i<=m; i++) {
    if (predicate(i)) {
      p.push(i)
    }
  }
  return sum(p)
}

// removes repetitions in a sorted list
function uniq<t>(l:t[]) {
  var last
  var result = []
  for (var e of l) {
    if (e !== last) {
      result.push(e)
      last = e
    }
  }
  return result
}

export function distinctPowers(m:number) {
  function f(a:bigint, b:bigint) {
    return a ** b
  }
  var r = []
  for (var a=BigInt(2); a<=m; a++) {
    for (var b=BigInt(2); b<=m; b++) {
      r.push(f(a, b))
    }
  }
  r.sort()
  return uniq(r).length
}

export function spiralDiagonals(m:number) {
  var s = [1]
  var side = 1
  var level = 2
  var start = 1
  var size = (level-1) * 2 + 1
  while (size <= m) {
    const d = start+size-1
    s.push(d)
    if (side===4) {
      side = 1
      level++
      start = d
    } else {
      side++
      start = d
    }
    size = (level-1) * 2 + 1
  }
  return sum(s)
}

export function quadraticPrimes(m:number) {
  var x = 0
  var y = 0
  for (var a=-m; a<=m; a++) {
    for (var b=-m; b<=m; b++) {
      const c = consecutivePrimes(a, b)
      if (c > x) {
        x = c
        y = a * b
      }
    }
  }
  return y
}

export function consecutivePrimes(a:number, b:number) {
  function f(n:number) {
    return n**2 + n * a + b
  }
  function p(n:number) {
    return isPrime(Math.abs(f(n)))
  }
  var n = 0
  while (p(n)) {
    n++
  }
  return n
}

export function reciprocalCycles(m: number) {
  let maxLen = 0
  let maxD = 1
  for (var d=1; d<m; d++) {
    const l = extractCycle(periodicDiv(1, d)).length
    if (l > maxLen) {
      maxLen = l
      maxD = d
    }
  }
  return maxD
}

export function extractCycle(l: number[]) {
  if (l.length == 0) {
    return []
  }
  function p(s: number[]): [number|undefined, number[]] {
    s = s.reverse()
    const r = s.pop()
    s = s.reverse()
    return [r, s]
  }
  let [t, l1] = p(l)
  let seen = [[t]]
  function elem() {
    return seen.filter(s => listEquals(s, l1)).length > 0
  }
  while (!elem() && l1.length>0) {
    [t, l1] = p(l1)
    seen.map(s => s.push(t))
    seen.push([t])
  }
  return l1
}

function listEquals<Type>(l1: Type[], l2: Type[]) {
  for (var i=0; i<l1.length; i++) {
    if (l1[i] != l2[i]) {
      return false
    }
  }
  return true
}

export function periodicDiv(n:number, m:number) {
  function count(l:number, t:number[]) {
    return t.filter(x => x===l).length
  }
  let r:number[] = []
  let e:number[] = []
  let n1 = n
  if (n<m) {
    n1 = n * 10
  }
  let [d, o] = divMod(n1, m)
  function predicate() {
    return o == 0 || (count(o, e) >= 2 && count(d, r) >= 2)
  }
  while (!predicate()) {
    r.push(d)
    e.push(o)
    n1 = o
    if (o <m) {
      n1 = o * 10
    }
    [d, o] = divMod(n1, m)
  }
  return r
}

export function digitFibonacci(m: number) {
  var fn2: bigint = BigInt(1)
  var fn1: bigint = BigInt(1)
  var f:bigint = BigInt(2)
  var i = 3
  function p (n: bigint) {
    return String(n).length >= m
  }
  while (!p(f)) {
    fn2 = fn1
    fn1 = f
    f = fn1+fn2
    i++
  }
  return i
}

export function lexicographicPermutations(options: string[], index: number) {
  const o = options.sort()
  const p = perm(o)
  return p[index-1]
}

export function perm(options: string[]): string[] {
  if (options.length == 0) {
    return []
  } else if (options.length == 1) {
    return options
  }
  function p(c: string): string[] {
    const o = options.filter(e => e != c)
    const r = perm(o)
    return r.map(e => c + e)
  }
  return options.flatMap(p)
}

export function nonAbundantSums() {
  const m = 28123
  function isAbundant(x:number) {
    return sum(properDivisors(x)) > x
  }
  var a : number[] = []
  for (var i=0; i<m; i++) {
    if (isAbundant(i)) {
      a.push(i)
    }
  }
  interface S {
    [index: number]: boolean
  }
  var sums : S = {}
  for (const a1 of a) {
    for (const a2 of a) {
      sums[a1+a2] = true
    }
  }
  function predicate(x:number) {
    return Boolean(sums[x])
  }
  var b = []
  for (var i=1; i<m; i++) {
    if (!predicate(i)) {
      b.push(i)
    }
  }
  return sum(b)
}

export async function namesScores() {
  const contents = await fs.readFile('0022_names.txt', 'utf8')
  function r (s:string) {
    return s.replaceAll('"', '')
  }
  const names = contents.split(',').map(r).sort()
  var scores = 0
  for (const n in names) {
    const a = alphabeticalValue(names[n])
    scores += a * (Number(n)+1)
  }
  return scores
}

export function alphabeticalValue(s:string) {
  function c (a:string) {
    return a.charCodeAt(0) - 64
  }
  return sum(s.split('').map(c))
}

export function amicable(m:number) {
  function s(n:number) {
    return sum(properDivisors(n))
  }
  function isAmicable(n:number) {
    const e = s(n)
    return s(e) == n && e < m && e != n
  }
  var a = []
  for (var i=1; i<m; i++) {
    if (isAmicable(i)) {
      a.push(i)
    }
  }
  return sum(a)
}

function properDivisors(n:number) {
  var d = []
  function predicate(u:number) {
    return n % u == 0
  }
  for (var i=1; i < n; i++) {
    if (predicate(i)) {
      d.push(i)
    }
  }
  return d
}

export function factorialDigitsSum(n:number) {
  return digitsSum(factorial(n))
}

function factorial(n:number) {
  var f = 1n
  for (var i=1n; i <= n; i++) {
    f = f * i
  }
  return f
}

export function sundays() {
  const start = dayjs('1901-01-01')
  const end   = dayjs('2000-12-31')
  function pred (d:dayjs.Dayjs) {
    return d.day() === 0 && d.date() === 1
  }
  var day = start
  var count = 0
  while (day <= end) {
    if (pred(day)) {
      count += 1
    }
    day = day.add(1, 'day')
  }
  return count
}

export function maximumPathSum () {
  const t = [[75],
             [95, 64],
             [17, 47, 82],
             [18, 35, 87, 10],
             [20,  4, 82, 47, 65],
             [19,  1, 23, 75,  3, 34],
             [88,  2, 77, 73,  7, 63, 67],
             [99, 65,  4, 28,  6, 16, 70, 92],
             [41, 41, 26, 56, 83, 40, 80, 70, 33],
             [41, 48, 72, 33, 47, 32, 37, 16, 94, 29],
             [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14],
             [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57],
             [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48],
             [63, 66,  4, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31],
             [ 4, 62, 98, 27, 23,  9, 70, 98, 73, 93, 38, 53, 60,  4, 23]]
  type PP = [number, number[]]
  function g (pp:PP):PP[] {
    const [point, path] = pp
    const path1 = path.slice()
    const path2 = path.slice()
    path1.push(point)
    path2.push(point+1)
    return [[point, path1], [point+1, path2]]
  }
  function p (i:number):PP[] {
    if (i===0) {
      return [[0, [0]]]
    } else {
      return p(i-1).flatMap(g)
    }
  }
  const allPaths = p(14)
  function access (pp:PP) {
    const [_point, path] = pp
    var values = []
    for (var i=0; i<=14; i++) {
      values.push(t[i][path[i]])
    }
    return sum(values)
  }
  return maximum(allPaths.map(access))
}

export function numberLetterCounts(m:number) {
  var s = 0
  for (var i=1; i<=m; i++) {
    const w = numberWords(i)
    function p (l:string) {
      return l != ' ' && l != '-'
    }
    const t = String(w).split('').filter(p)
    s += t.length
  }
  return s
}

function numberWords(n:number):string {
  switch (n) {
  case 1000:
    return "one thousand"
    break;
  case 1:
    return "one"
    break;
  case 2:
    return "two"
    break;
  case 3:
    return "three"
    break;
  case 4:
    return "four"
    break;
  case 5:
    return "five"
    break;
  case 6:
    return "six"
    break;
  case 7:
    return "seven"
    break;
  case 8:
    return "eight"
    break;
  case 9:
    return "nine"
    break;
  case 10:
    return "ten"
    break;
  case 11:
    return "eleven"
    break;
  case 12:
    return "twelve"
    break;
  case 13:
    return "thirteen"
    break;
  case 14:
    return "fourteen"
    break;
  case 15:
    return "fifteen"
    break;
  default:
    function e (p:number, q:number):string {
      function f (a:string, b:number):string {
        if (b===0) {
          return a
        } else {
          return a + "-" + numberWords(b)
        }
      }
      switch (p) {
      case 1:
        if (q === 8) {
          return "eighteen"
        } else {
          return numberWords(q) + "teen"
        }
        break;
      case 2:
        return f("twenty", q)
        break;
      case 3:
        return f("thirty", q)
        break;
      case 4:
        return f("forty", q)
        break;
      case 5:
        return f("fifty", q)
        break;
      case 6:
        return f("sixty", q)
        break;
      case 7:
        return f("seventy", q)
        break;
      case 8:
        return f("eighty", q)
        break;
      case 9:
        return f("ninety", q)
        break;
      default:
        throw(p)
        break;
      }
    }
    const [h, r1] = divMod(n, 100)
    const [t, r2] = divMod(r1, 10)
    function v ():string {
      if(r1 < 16) {
        return numberWords(r1)
      } else {
        return e(t, r2)
      }
    }
    if (h>0) {
      if (r1>0) {
        return numberWords(h) + " hundred and " + v()
      } else {
        return numberWords(h) + " hundred"
      }
    } else {
      return v()
    }
    break;
  }
}

function divMod (n:number, d:number) {
  return [Math.trunc(n/d), n%d]
}

function digitsSum(n:bigint) {
  const l = String(n).split('')
  return sum(l.map(Number))
}

export function powerDigitsSum(e:bigint) {
  const n = BigInt(2) ** e
  return digitsSum(n)
}

export function latticePaths(s:number) {
  interface P {
    [index: string]: number
  }
  var p:P = {}
  function k (x:number, y:number) {
    return [x, y].join(',')
  }
  function f (x:number, y:number):number {
    if (x===0 || y===0) {
      return 1
    } else if (k(x, y) in p) {
      return p[k(x, y)]
    } else {
      const z = f(x-1, y) + f (x, y-1)
      p[k(x, y)] = z
      return z
    }
  }
  var r = 2
  for (var i=1; i<=s; i++) {
    for (var j=1; j<=s; j++) {
      r = f(i, j)
    }
  }
  return r
}

export function longestCollatz(m:number) {
  interface L {
    [index: number]: number;
  }
  var l:L = {}
  var k = []
  for (var i=1; i<m; i++) {
    const h = collatzLength(i)
    l[h] = i
    k.push(h)
  }
  const a = maximum(k)
  return l[a]
}

function maximum(a:number[]) {
  return a.reduce((a, b) => Math.max(a, b), -Infinity)
}

export function collatzLength(s:number) {
  function next(n:number) {
    if (even(n)) {
      return n/2
    } else {
      return 3*n+1
    }
  }
  var c = [s]
  while (s>1) {
    s = next(s)
    c.push(s)
  }
  return c.length
}

export function largeSum() {
  const n:number[] =
        [ 37107287533902102798797998220837590246510135740250,
          46376937677490009712648124896970078050417018260538,
          74324986199524741059474233309513058123726617309629,
          91942213363574161572522430563301811072406154908250,
          23067588207539346171171980310421047513778063246676,
          89261670696623633820136378418383684178734361726757,
          28112879812849979408065481931592621691275889832738,
          44274228917432520321923589422876796487670272189318,
          47451445736001306439091167216856844588711603153276,
          70386486105843025439939619828917593665686757934951,
          62176457141856560629502157223196586755079324193331,
          64906352462741904929101432445813822663347944758178,
          92575867718337217661963751590579239728245598838407,
          58203565325359399008402633568948830189458628227828,
          80181199384826282014278194139940567587151170094390,
          35398664372827112653829987240784473053190104293586,
          86515506006295864861532075273371959191420517255829,
          71693888707715466499115593487603532921714970056938,
          54370070576826684624621495650076471787294438377604,
          53282654108756828443191190634694037855217779295145,
          36123272525000296071075082563815656710885258350721,
          45876576172410976447339110607218265236877223636045,
          17423706905851860660448207621209813287860733969412,
          81142660418086830619328460811191061556940512689692,
          51934325451728388641918047049293215058642563049483,
          62467221648435076201727918039944693004732956340691,
          15732444386908125794514089057706229429197107928209,
          55037687525678773091862540744969844508330393682126,
          18336384825330154686196124348767681297534375946515,
          80386287592878490201521685554828717201219257766954,
          78182833757993103614740356856449095527097864797581,
          16726320100436897842553539920931837441497806860984,
          48403098129077791799088218795327364475675590848030,
          87086987551392711854517078544161852424320693150332,
          59959406895756536782107074926966537676326235447210,
          69793950679652694742597709739166693763042633987085,
          41052684708299085211399427365734116182760315001271,
          65378607361501080857009149939512557028198746004375,
          35829035317434717326932123578154982629742552737307,
          94953759765105305946966067683156574377167401875275,
          88902802571733229619176668713819931811048770190271,
          25267680276078003013678680992525463401061632866526,
          36270218540497705585629946580636237993140746255962,
          24074486908231174977792365466257246923322810917141,
          91430288197103288597806669760892938638285025333403,
          34413065578016127815921815005561868836468420090470,
          23053081172816430487623791969842487255036638784583,
          11487696932154902810424020138335124462181441773470,
          63783299490636259666498587618221225225512486764533,
          67720186971698544312419572409913959008952310058822,
          95548255300263520781532296796249481641953868218774,
          76085327132285723110424803456124867697064507995236,
          37774242535411291684276865538926205024910326572967,
          23701913275725675285653248258265463092207058596522,
          29798860272258331913126375147341994889534765745501,
          18495701454879288984856827726077713721403798879715,
          38298203783031473527721580348144513491373226651381,
          34829543829199918180278916522431027392251122869539,
          40957953066405232632538044100059654939159879593635,
          29746152185502371307642255121183693803580388584903,
          41698116222072977186158236678424689157993532961922,
          62467957194401269043877107275048102390895523597457,
          23189706772547915061505504953922979530901129967519,
          86188088225875314529584099251203829009407770775672,
          11306739708304724483816533873502340845647058077308,
          82959174767140363198008187129011875491310547126581,
          97623331044818386269515456334926366572897563400500,
          42846280183517070527831839425882145521227251250327,
          55121603546981200581762165212827652751691296897789,
          32238195734329339946437501907836945765883352399886,
          75506164965184775180738168837861091527357929701337,
          62177842752192623401942399639168044983993173312731,
          32924185707147349566916674687634660915035914677504,
          99518671430235219628894890102423325116913619626622,
          73267460800591547471830798392868535206946944540724,
          76841822524674417161514036427982273348055556214818,
          97142617910342598647204516893989422179826088076852,
          87783646182799346313767754307809363333018982642090,
          10848802521674670883215120185883543223812876952786,
          71329612474782464538636993009049310363619763878039,
          62184073572399794223406235393808339651327408011116,
          66627891981488087797941876876144230030984490851411,
          60661826293682836764744779239180335110989069790714,
          85786944089552990653640447425576083659976645795096,
          66024396409905389607120198219976047599490197230297,
          64913982680032973156037120041377903785566085089252,
          16730939319872750275468906903707539413042652315011,
          94809377245048795150954100921645863754710598436791,
          78639167021187492431995700641917969777599028300699,
          15368713711936614952811305876380278410754449733078,
          40789923115535562561142322423255033685442488917353,
          44889911501440648020369068063960672322193204149535,
          41503128880339536053299340368006977710650566631954,
          81234880673210146739058568557934581403627822703280,
          82616570773948327592232845941706525094512325230608,
          22918802058777319719839450180888072429661980811197,
          77158542502016545090413245809786882778948721859617,
          72107838435069186155435662884062257473692284509516,
          20849603980134001723930671666823555245252804609722,
          53503534226472524250874054075591789781264330331690 ]
  const s = sum(n)
  return String(s).split('').slice(0, 11).join('')
}

export function highlyDivisibleTriangularNumber(m:number) {
  var t = 1
  var i = 2
  while (divisors(t).length < m) {
    t += i
    i += 1
  }
  return t
}

export function divisors(n:number) {
  var d = [1]
  for (var i=2; i<=n/2; i++) {
    if (n % i === 0) {
      d.push(i)
    }
  }
  d.push(n)
  return d
}

export function productInAGrid() {
  const g = [  8,  2, 22, 97, 38, 15,  0, 40,  0, 75,  4,  5,  7, 78, 52, 12, 50, 77, 91,  8
            , 49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, 69, 48,  4, 56, 62,  0
            , 81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, 30,  3, 49, 13, 36, 65
            , 52, 70, 95, 23,  4, 60, 11, 42, 69, 24, 68, 56,  1, 32, 56, 71, 37,  2, 36, 91
            , 22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80
            , 24, 47, 32, 60, 99,  3, 45,  2, 44, 75, 33, 53, 78, 36, 84, 20, 35, 17, 12, 50
            , 32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70
            , 67, 26, 20, 68,  2, 62, 12, 20, 95, 63, 94, 39, 63,  8, 40, 91, 66, 49, 94, 21
            , 24, 55, 58,  5, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72
            , 21, 36, 23,  9, 75,  0, 76, 44, 20, 45, 35, 14,  0, 61, 33, 97, 34, 31, 33, 95
            , 78, 17, 53, 28, 22, 75, 31, 67, 15, 94,  3, 80,  4, 62, 16, 14,  9, 53, 56, 92
            , 16, 39,  5, 42, 96, 35, 31, 47, 55, 58, 88, 24,  0, 17, 54, 24, 36, 29, 85, 57
            , 86, 56,  0, 48, 35, 71, 89,  7,  5, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58
            , 19, 80, 81, 68,  5, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77,  4, 89, 55, 40
            ,  4, 52,  8, 83, 97, 35, 99, 16,  7, 97, 57, 32, 16, 26, 26, 79, 33, 27, 98, 66
            , 88, 36, 68, 87, 57, 62, 20, 72,  3, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69
            ,  4, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18,  8, 46, 29, 32, 40, 62, 76, 36
            , 20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74,  4, 36, 16
            , 20, 73, 35, 29, 78, 31, 90,  1, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57,  5, 54
            ,  1, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52,  1, 89, 19, 67, 48
            ]
  var iVertical = []
  var iHorizontal = []
  var iDiagonal = []
  var iDiagonal2 = []
  for (var i=0; i<=399-60; i++) {
    iVertical.push([i, i+20, i+40, i+60])
  }
  function hasFourToTheRight (i:number) {
    return ((i+1) % 20) < 18
  }
  for (var i=0; i<=399-3; i++) {
    if (hasFourToTheRight(i)) {
      iHorizontal.push([i, i+1, i+2, i+3])
    }
  }
  for (var i=0; i<=399-63; i++) {
    if (hasFourToTheRight(i)) {
      iDiagonal.push([i, i+21, i+42, i+63])
    }
  }
  for (var i=59; i<=399-3; i++) {
    if (hasFourToTheRight(i)) {
      iDiagonal2.push([i, i-19, i-38, i-57])
    }
  }
  const indexes = iVertical.concat(iHorizontal).concat(iDiagonal2).concat(iDiagonal2)
  function value(i:number) {
    return g[i]
  }
  function p(i:number[]) {
    return product(i.map(value))
  }
  return Math.max(...indexes.map(p))
}

export function summationOfPrimes(m:number) {
  var s = 0
  for (var n=2; n<m; n++) {
    if (isPrime(n)) {
      s += n
    }
  }
  return s
}

export function pythagoreanTriplet(g:number) {
  for (var a=1; a<g; a++) {
    for (var b=1; b<g; b++) {
      for (var c=1; c<g; c++) {
        if (a+b+c==g && a**2+b**2==c**2) {
          return a*b*c
        }
      }
    }
  }
}

export function productInSeries(n:number) {
  const n1000 = '7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450'
  const digits = n1000.split('')
  function grouped(s:string[]): string[][] {
    var g = []
    var t = s
    while (t.length >= n) {
      const u = t.slice(0, n)
      g.push(u)
      t = t.slice(1)
    }
    return g
  }
  function p(s:string[]): number {
    const b = s.map(r => Number(r))
    return product(b)
  }
  const d = grouped(digits).map(p)
  return Math.max(...d)
}

function product(a:number[]) {
  return a.reduce((b:number, c:number) => b * c, 1)
}

export function sum(a:number[]) {
  return a.reduce((b:number, c:number) => b + c, 0)
}

export function nthPrime(n:number) {
  var s = 2
  var i = 1
  while (i <= n) {
    if (isPrime(s)) {
       i += 1
    }
    s += 1
  }
  return (s-1)
}

export function isPrime(n:number) {
  if (n<2) {
    return false
  }
  const m = Math.sqrt(n)
  var t = 2
  var i = true
  while (t <= m && i) {
    const o = n % t
    i = o != 0
    t += 1
  }
  return i
}

export function sumSquareDifference(m:number) {
  let sumSquares = 0
  let almostSquareSum = 0
  for (var i=1; i <= m; i++) {
    sumSquares += i**2
    almostSquareSum += i
  }
  return (almostSquareSum**2) - sumSquares
}

export function smallestMultiple(m:number) {
  function evenly(n:number) {
    let all = true
    let d = 1
    while (all && d <= m) {
      all = (n%d) == 0
      d++
    }
    return all
  }
  let u = m
  while (true) {
    if(evenly(u)) {
      return u
    } else {
      u++
    }
  }
}

export function largestPalindromeProduct(n:number) {
  var cl = []
  for (var a=100; a<1000; a++) {
    for (var b=100; b<1000; b++) {
      const c = a * b
      if (isPalindrome(String(c))) {
        cl.push(c)
      }
    }
  }
  return Math.max(...cl)
}

export function isPalindrome (s:string) {
  return s.split('').reverse().join('') == s
}

export function largestPrimeFactor(n:number) {
  const m = Math.sqrt(n)
  var t = 2
  var f
  while (t <= m) {
    const o = n % t
    if (o == 0) {
      n = n / t
      f = t
    }
    t += 1
  }
  return f
}

export function f2(n:number) {
  interface A {
    [index: number]: number;
  }
  var a : A = {
    0: 1,
    1: 1,
    2: 2
  }
  var evens = 0
  var i = 2
  var j = 2
  while (j<n) {
    var v1 = a[i-1]
    var v2 = a[i-2]
    j = v1+v2
    a[i] = j
    if (even(j)) {
      evens += j
    }
    i += 1
  }
  return evens
}

function even(n:number) {
  return n % 2 === 0
}

export function f1 (n:number) {
  var l=[]
  function sum(a:number, b:number) {
    return a + b
  }
  for (var i=0; i<n; i++) {
    if (i % 3 == 0) {
      l.push(i)
    }
    else if (i % 5 == 0) {
      l.push(i)
    }
  }
  return l.reduce(sum, 0)
}
