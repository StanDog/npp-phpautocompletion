<?php

	// Single line comment
	class foo
	{
		public $attr1;  // One more comment
		public static $attr2;
		const constant1=3.14;
		var $experimentally_supported; /* multiple
		line "comment with
		quotation marks*/
		private $not_visible_if_unset_in_settings1;
		protected $not_visible_if_unset_in_settings2;

		public function bar($param1, array $param2, $param3="hello")
		{
			echo $param1;
			$tmp = 'He said: "public $tmp2;" was never public $tmp3; ok?';
		}

		/*static*/ public function bar2()
		{
		}

		protected function inheritable()
		{
		}

		private static function i_am_not_inheritable()
		{
		}
	}
	class bar extends foo
	{
		public $中國;
		public $Россия;
		public $にほん;
		public $중국;
		public $ابتثجحخدذرزسشصضطظعغفقكلمنهوية;
		public $עברית;

		public function 機能($param)
		{
			echo $param;
		}

		public function bar2($newparam) {} // overloaded from foo
	}

	$b = new bar();
	echo $b::constant1 ."<br>";
	echo $b->機能("cat") ."<br>";
	$b->bar("dog", array());

?>