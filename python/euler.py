
from datetime import date, timedelta
import functools
import itertools
import math

def integer_right_triangles():
  def solutions(p):
    s = []
    for a in range(1, p):
      for b in range(a, p):
        if 2*(a+b) == p + 2*a*b/p:
          s.append({a, b})
    s.sort()
    return len(uniq(s))
  ps = list(range(1, 1000))
  ps.sort(key=solutions)
  return ps.pop()

def pandigital_multiples(m1, m2):
  def is_pandigital(s):
    l = list(s)
    l.sort()
    return l == ['1', '2', '3', '4', '5', '6', '7', '8', '9']
  def multiple(n, l):
    p = map(lambda x: str(x*n), l)
    return functools.reduce((lambda x, y: x+y), p, '')
  multiples = []
  for i in range(1, m1):
    for j in range(1, m2):
      l = range(1, j)
      s = multiple(i, l)
      if is_pandigital(s):
        multiples.append(int(s))
  return max(multiples)

def truncatable_primes():
  def predicate(n):
    def l(s):
      if s == '':
        return True
      s2 = s[1:]
      if s2 == '':
        return True
      if is_prime(int(s2)):
        return l(s2)
      else:
        return False
    def r(s):
      if s == '':
        return True
      s2 = s[:-1]
      if s2 == '':
        return True
      if is_prime(int(s2)):
        return r(s2)
      else:
        return False
    s = str(n)
    return is_prime(n) and l(s) and r(s)
  s = 0
  c = 0
  i = 10
  while c<11:
    if predicate(i):
      s += i
      c += 1
    i += 1
  return s

def double_base_palindromes():
  def predicate(n):
    b10 = is_palindrome(str(n))
    b2  = is_palindrome("{:b}".format(n))
    return b10 and b2
  s = 0
  for i in range(1, 1000000):
    if predicate(i):
      s += i
  return s

def circular_primes(m):
  def cycles(n):
    def c(s):
      (a, b) = split_at(s, 1)
      return b + a
    t = str(n)
    y = []
    for i in range(0, len(t)):
      t = c(t)
      y.append(t)
    return y
  def predicate(n):
    c = cycles(n)
    for e in c:
      if not is_prime(int(e)):
        return False
    return True
  p = 0
  for i in range(2, m):
    if predicate(i):
      p += 1
  return p

def digit_factorials(m):
  def is_digit_factorial(n):
    def f(t): return factorial(int(t))
    s = sum(map(f, list(str(n))))
    return s == n
  r = 0
  for i in range(10, m+1):
    if is_digit_factorial(i):
      r += i
  return r

def digit_cancelling_fractions():
  def is_digit_cancelling(n, d):
    def f(b, g):
      n2 = int(str(n).replace(g, '', 1))
      d2 = int(str(d).replace(g, '', 1))
      l = len(str(n2)) < len(str(n))
      if d2==0:
        e = False
      else:
        e = n2/d2 == n/d
      return b or (e and l)
    return functools.reduce(f, "123456789", False)
  c = []
  for n in range(10, 99):
    for d in range(n+1, 100):
      if is_digit_cancelling(n, d):
        c.append((n, d))
  return c

def pandigital_products():
  pe = itertools.permutations("123456789")
  products = []
  for p in pe:
    for i in range(1, 9):
      for j in range(1, 9-i):
        (p1, p2) = split_at(tuple_to_str(p), i)
        (p21, p22) = split_at(p2, j)
        (a, b, c) = (int(p1), int(p21), int(p22))
        if a * b == c:
          products.append(c)
  products.sort()
  return sum(uniq(products))
        
def tuple_to_str(t): return ''.join(t)

def split_at(s, n):
  s1 = s[0:n]
  s2 = s[n:len(s)]
  return (s1, s2)

def coin_sums():
  def f(c, t):
    if len(c) == 0:
      if t == 0:
        return 1
      else:
        return 0
    n = c.pop()
    count = 0
    slots = t // n
    for i in range(0, slots+1):
      value = i * n
      count += f(c[:], t - value)
    return count
  return f([1, 2, 5, 10, 20, 50, 100, 200], 200)

def fifth_powers(m):
  def predicate(n):
    def g(x): return int(x) ** 5
    p = sum(map(g, str(n)))
    return p == n
  return sum(list(filter(predicate, range(2, m))))

def uniq(l):
  last = None
  r = []
  for e in l:
    if e != last:
      r.append(e)
      last = e
  return r

def distinct_powers(m):
  def f(a, b): return a ** b
  r = []
  for a in range(2, m+1):
    for b in range(2, m+1):
      r.append(f(a, b))
  r.sort()
  return len(uniq(r))

def spiral_diagonals(m):
  s = [1]
  side = 1
  level = 2
  start = 1
  size = (level-1) * 2 + 1
  while size <= m:
    d = start+size-1
    s.append(d)
    if side == 4:
      side = 1
      level += 1
    else:
      side += 1
    start = d
    size = (level-1) * 2 + 1
  return sum(s)

def quadratic_primes(m):
  x = 0
  y = 0
  for a in range(-m, m+1):
    for b in range(-m, m+1):
      c = consecutive_primes(a, b)
      if c>x:
        x = c
        y = a * b
  return y

def consecutive_primes(a, b):
  def f(n): return n**2 + n*a + b
  def p(n): return is_prime(abs(f(n)))
  n = 0
  while p(n): n+=1
  return n

def reciprocal_cycles(m):
  max_len = 0
  max_d = 1
  for d in range(1, m):
    l = len(extract_cycle(periodic_div(1, d)))
    if l > max_len:
      max_len = l
      max_d = d
  return max_d

def extract_cycle(l):
  if l == []: return []
  def p(s):
    s.reverse()
    r = s.pop()
    s.reverse()
    return (r, s)
  (t, l1) = p(l)
  seen = [[t]]
  def elem(p, s): return len(list(filter(p, s))) > 0
  while not elem(lambda x: x==l1, seen) and len(l1)>0:
    (t, l1) = p(l1)
    for s in seen: s.append(t)
    seen.append([t])
  return l1

def periodic_div(n, m):
  def count(l, t): return len(list(filter(lambda x: x==l, t)))
  def predicate(): return o == 0 or (count(o, e) >= 2 and count(d, r) >= 2)
  r = []
  e = []
  n1 = n
  if n < m:
    n1 = n * 10
  (d, o) = divmod(n1, m)
  while not predicate():
    r.append(d)
    e.append(o)
    n1 = o
    if o < m:
      n1 = o * 10
    (d, o) = divmod(n1, m)
  return r

def digit_fibonacci(m):
  fn2 = 1
  fn1 = 1
  f = 2
  i = 3
  def p(n): return len(str(n)) >= m
  while not p(f):
    fn2 = fn1
    fn1 = f
    f = fn1+fn2
    i += 1
  return i

def lexicographic_permutations(o, i):
  p = list(itertools.permutations(o))
  return functools.reduce(lambda x, y: x+y, p[i-1])

def non_abundant_sums():
  def is_abundant(x): return sum(proper_divisors(x)) > x
  m = 28123
  a = list(filter(is_abundant, range(12, m+1)))
  sums = set()
  for a1 in a:
    for a2 in a:
      sums.add(a1+a2)
  def predicate(x): return x not in sums
  return sum(filter(predicate, range(1, m+1)))

def names_scores():
  with open('0022_names.txt') as f: contents = f.read()
  names = []
  for c in contents.split(','): names.append(c.strip('"'))
  names.sort()
  scores = 0
  for i in range(len(names)):
    n = names[i]
    a = alphabetical_value(n)
    scores += a * (i+1)
  return scores

def alphabetical_value(s):
  def f(c): return ord(c) - 64
  return sum(map(f, s))

def amicable(m):
  def s(n): return sum(proper_divisors(n))
  def is_amicable(n):
    e = s(n)
    return s(e) == n and e < m and e != n
  a = []
  for i in range(1, m):
    if is_amicable(i):
      a.append(i)
  return sum(a)

def proper_divisors(n):
  d = []
  def predicate(u): return n % u == 0
  for i in range(1, n):
    if predicate(i):
      d.append(i)
  return d

def factorial_digits_sum(n):
  return digits_sum(factorial(n))

def factorial(n):
  f = 1
  for i in range(1, n+1):
    f = f * i
  return f

def sundays():
  start = date.fromisoformat('1901-01-01')
  end   = date.fromisoformat('2000-12-31')
  def pred (d): return d.day == 1 and d.weekday() == 6
  day = start
  count = 0
  while day <= end:
    if pred(day):
      count += 1
    day = day + timedelta(days=1)
  return count

def maximum_path_sum():
  t = [[75],
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
  def g (pp):
    (point, path) = pp
    path1 = path[:]
    path2 = path[:]
    path1.append(point)
    path2.append(point+1)
    return [(point, path1), (point+1, path2)]
  def p (i):
    if (i==0):
      return [(0, [0])]
    else:
      r = []
      for gg in map(g, p(i-1)):
        for pp in gg:
          r.append(pp)
      return r
  all_paths = p(14)
  def access (pp):
    (_point, path) = pp
    values = []
    for i in range(15):
      values.append(t[i][path[i]])
    return sum(values)
  return max(map(access, all_paths))

def number_letter_counts(m):
  s = 0
  for i in range(1, m+1):
    w = number_words(i)
    def p (l):
      return l != ' ' and l != '-'
    t = []
    for c in filter(p, w): t.append(c)
    s += len(t)
  return s

def number_words(n):
  match n:
    case 1000:
      return "one thousand"
    case 1:
      return "one"
    case 2:
      return "two"
    case 3:
      return "three"
    case 4:
      return "four"
    case 5:
      return "five"
    case 6:
      return "six"
    case 7:
      return "seven"
    case 8:
      return "eight"
    case 9:
      return "nine"
    case 10:
      return "ten"
    case 11:
      return "eleven"
    case 12:
      return "twelve"
    case 13:
      return "thirteen"
    case 14:
      return "fourteen"
    case 15:
      return "fifteen"
    case _:
      def e(p, q):
        def f(a, b):
          if b==0:
            return a
          else:
            return a + "-" + number_words(b)
        match p:
          case 1:
            if q==8:
              return "eighteen"
            else:
              return number_words(q) + "teen"
          case 2:
            return f("twenty", q)
          case 3:
            return f("thirty", q)
          case 4:
            return f("forty", q)
          case 5:
            return f("fifty", q)
          case 6:
            return f("sixty", q)
          case 7:
            return f("seventy", q)
          case 8:
            return f("eighty", q)
          case 9:
            return f("ninety", q)
          case _:
            throw(p)
      (h, r1) = divmod(n, 100)
      (t, r2) = divmod(r1, 10)
      def v():
        if r1 < 16:
          return number_words(r1)
        else:
          return e(t, r2)
      if h>0:
        if r1>0:
          return number_words(h) + " hundred and " + v()
        else:
          return number_words(h) + " hundred"
      else:
          return v()

def digits_sum(n): return sum(map(int, str(n)))

def power_digits_sum(e): return digits_sum(2 ** e)

def lattice_paths(s):
  p = {}
  def k(x, y): return str(x) + ',' + str(y)
  def f(x, y):
    if x==0 or y==0:
      return 1
    elif k(x, y) in p:
      return p[k(x, y)]
    else:
      z = f(x-1, y) + f(x, y-1)
      p[k(x,y)] = z
      return z
  r = 2
  for i in range(1, s+1):
    for j in range(1, s+1):
      r = f(i, j)
  return r

def longest_collatz(m):
  l = {}
  for i in range(1, m):
    l[collatz_length(i)] = i
  a = max(l.keys())
  return l[a]

def collatz_length(n):
  def next(x):
    if even(x):
      return x/2
    else:
      return 3*x+1
  c = [n]
  while n > 1:
    n = next(n)
    c.append(n)
  return len(c)

def large_sum():
  n = [ 37107287533902102798797998220837590246510135740250,
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
  s = sum(n)
  return str(s)[0:10]

def highly_divisible_triangular_numbers(m):
  t = 1
  i = 2
  while len(divisors(t)) < m:
    t += i
    i += 1
  return t

def divisors(n):
  d = [1]
  i = 2
  while i <= n/2:
    if n % i == 0:
      d.append(i)
    i += 1
  d.append(n)
  return d

def product_in_a_grid():
  g = [  8,  2, 22, 97, 38, 15,  0, 40,  0, 75,  4,  5,  7, 78, 52, 12, 50, 77, 91,  8
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
  i_vertical = []
  i_horizontal = []
  i_diagonal = []
  i_diagonal_2 = []
  for x in range(0, 399-59):
    i_vertical.append([x, x+20, x+40, x+60])
  def has_four_to_the_right(x):
    return ((x+1) % 20) < 18
  for x in range(0, 399-2):
    if has_four_to_the_right(x):
      i_horizontal.append([x, x+1, x+2, x+3])
  for x in range(0, 399-62):
    if has_four_to_the_right(x):
      i_diagonal.append([x, x+21, x+42, x+63])
  for x in range(59, 399-2):
    if has_four_to_the_right(x):
      i_diagonal_2.append([x, x-19, x-38, x-57])
  i = i_vertical + i_horizontal + i_diagonal + i_diagonal_2
  def p(indexes):
    def value(index): return g[index]
    return math.prod(map(value, indexes))
  return max(map(p, i))

def summation_of_primes(m):
  s = []
  for n in range(2, m):
    if is_prime(n):
      s.append(n)
  return sum(s)

def pythagorean_triplet():
  for a in range(1, 1000):
    for b in range(1, 1000):
      for c in range(1, 1000):
        if a+b+c==1000 and a**2+b**2==c**2:
          return a*b*c

def product_in_series(n):
  n1000 = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
  def grouped(t):
    g = []
    while len(t) >= n:
      g.append(map(int, t[0:n]))
      t = t[1:]
    return g
  return max(map(math.prod, grouped(n1000)))

def nth_prime(n):
  s = 2
  i = 1
  while i <= n:
    if is_prime(s):
      i += 1
    s += 1
  return (s-1)

def is_prime(n):
  if n < 2:
    return False
  m = math.sqrt(n)
  t = 2
  while t <= m:
    o = n % t
    if o == 0:
      return False
    t += 1
  return True

def sum_square_difference(m):
  n = range(1, m+1)
  def s(n): return n**2
  return s(sum(n)) - sum(map(s, n))

def smallest_multiple(m):
  def evenly(n):
    d = 1
    a = True
    while a and d <= m:
      a = (n%d) == 0
      d += 1
    return a
  u = m
  while True:
    if evenly(u):
      return u
    else:
      u += 1

def largest_palindrome_product():
  al = range(100, 999)
  bl = range(100, 999)
  cl = []
  for a in al:
    for b in bl:
      c = a*b
      if is_palindrome(str(c)):
        cl.append(c)
  return max(cl)

def is_palindrome(s): return s[::-1] == s

def prime_factors(n):
  m = math.sqrt(n)
  t = 2
  f = []
  while t <= m:
    (d, o) = divmod(n, t)
    if o == 0:
      n = d
      f.append(t)
    t += 1
  return f

def f2(n):
  a = {
    0: 1,
    1: 1,
    2: 2
  }
  evens = 0
  i = 2
  j = 2
  while j < n:
    v1 = a[i-1]
    v2 = a[i-2]
    j = v1+v2
    a[i] = j
    if even(j): evens += j
    i += 1
  return evens
      
def even(n): return n % 2 == 0

def f1(n):
  l = []
  for i in range(n):
    if i % 3 == 0:
      l.append(i)
    elif i % 5 == 0:
      l.append(i)
  return sum(l)

