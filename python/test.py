import euler
import unittest

class Test(unittest.TestCase):
    def test_split_at(self):
        self.assertEqual(
            euler.split_at('abc', 1),
            ('a', 'bc'))
    def test_reciprocal_cycles(self):
        self.assertEqual(
            euler.reciprocal_cycles(10),
            7)
    def test_extract_cycle(self):
        self.assertEqual(
            euler.extract_cycle([1,4,2,8,5,7,1,4,2,8,5,7]),
            [1,4,2,8,5,7])
        self.assertEqual(
            euler.extract_cycle([1,6,6]),
            [6])
        self.assertEqual(euler.extract_cycle([]), [])
    def test_periodic_div(self):
        # this test results have several warts but contain the cycles
        self.assertEqual(
            euler.periodic_div(1, 7),
            [1,4,2,8,5,7,1,4,2,8,5,7])
        self.assertEqual(
            euler.periodic_div(1, 6),
            [1,6,6])
        self.assertEqual(
            euler.periodic_div(1, 156),
            [0,0,6,4,1,0,2,5,6,4,1,0,2,5])
        self.assertEqual(
            euler.periodic_div(1, 5),
            [])
        self.assertEqual(
            euler.periodic_div(1, 3),
            [3, 3])
    def test_digit_fibonacci(self):
        self.assertEqual(
            euler.digit_fibonacci(3),
            12)
    def test_lexicographic_permutations(self):
        self.assertEqual(
            euler.lexicographic_permutations('012', 5),
            '201')
    def test_proper_divisors(self):
        self.assertEqual(euler.proper_divisors(12), [1,2,3,4,6])
    def test_alphabetical_value(self):
        self.assertEqual(euler.alphabetical_value('COLIN'), 53)
    def test_number_letter_counts(self):
        self.assertEqual(euler.number_letter_counts(5), 19)
    def test_power_digits_sum(self):
        self.assertEqual(euler.power_digits_sum(15), 26)
    def test_lattice_paths(self):
        self.assertEqual(euler.lattice_paths(2), 6)
    def test_nth_prime(self):
        self.assertEqual(euler.nth_prime(6), 13)
    def test_sum_square_difference(self):
        self.assertEqual(euler.sum_square_difference(10), 2640)
    def test_smallest_multiple(self):
        self.assertEqual(euler.smallest_multiple(10), 2520)
    def test_is_palindrome(self):
        self.assertEqual(euler.is_palindrome(9009), True)
    def test_prime_factors(self):
        self.assertEqual(euler.prime_factors(13195), [5, 7, 13, 29])
    def test_f1(self):
        self.assertEqual(euler.f1(10), 23)

unittest.main()
