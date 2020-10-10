#![allow(non_snake_case)] // sorry, need this because this is a rewrite of existing Java code

//! # Software and constants to compute the zobrist hash
//!
//! The index in the zrandoms table is like `pxxxffffff`
//!  
//! Computes the zobrist constant for a certain piece (1..6) of a
//! certain player on a certain square. Arguments are not checked.
//!
//! To encode flag bits use `ppfZobrist()`.
//!  
//!  * player 0 or 1 for BLACK or WHITE
//!  * piece  1 to 6 for PAWN to KING
//!  * index  0 to 255 for A1 to H8

pub const fn ppfZobrist(player: u32, piece: u32, index: u32) -> u64 {
	ZRANDOMS[(((player * 6 + piece) << 6) + index) as usize]
}

/**
 * Convenience to get zobrist keys for flag bits (i.e. en-passant
 * and castling information)
 */
pub const fn flagZobrist(index: u32) -> u64 { ppfZobrist(0, 0, index) }

/// 1000 random long numbers for computation of Zobrist hash keys
/// We need only 832, but hey ...
#[rustfmt::skip]
const ZRANDOMS: [u64; 1000] = [
	0x29e48473df053401u64, 0x436328cac0eb14ffu64, 0x94df472ce2bfc808u64, 0xfae82f66506c5e5bu64,
	0xdb9533eb28d7d1cau64, 0xeed84e170d796a72u64, 0x5906d39eb751b57au64, 0x40e99acbffd458c1u64,
	0x273be4babd76ac77u64, 0xdf2c0fb73f664547u64, 0x4c9c77ab65735ec3u64, 0x54a0eac3783fc85fu64,
	0xf843100082dc36d9u64, 0x5df28348d96f3844u64, 0x988303a7324925abu64, 0xea48dd88e6d354e2u64,
	0xdc814d4f1bc3a8cau64, 0x823bdac94369523fu64, 0x3118eac3c19b802du64, 0x0aad812db43efc0cu64,
	0xd81d7eda97cdc210u64, 0x0ab05fa14d5d9f9au64, 0xd7f1225942dce6a8u64, 0x6cc0e8dc3dd257e5u64,
	0x5206013085c57895u64, 0x344628d0b9b713e0u64, 0x8cf903a4bfba0ecau64, 0xad8fcf2eae49d993u64,
	0x84c5fa820ecc543cu64, 0x59839488223a2ddau64, 0xc7405eccfef2cb09u64, 0xbbb5bb5a13055261u64,
	0xa7c4d3a6f311ae25u64, 0xf26a9eab70b34a7fu64, 0x50209ba6d3f55129u64, 0x19ef1e8a0357c208u64,
	0x0e5702e244506f88u64, 0xb81289345c249da4u64, 0x60df65c51999e930u64, 0x72b59da413cc4425u64,
	0xccef8c5484b5323eu64, 0x650bf04724e3966cu64, 0x75de44651dc00dd7u64, 0xc2d186360946f37au64,
	0x8b06b62560e44fa5u64, 0xa54e78d6131bf912u64, 0xcba4e9b9cbb86d34u64, 0x96e6bfa59c99f5ceu64,
	0x1b7dd30651a34294u64, 0xd844d6ae8de36fc2u64, 0xd6306da1bf314951u64, 0xfe07c04083519a3bu64,
	0xa6e69bbe358d4af2u64, 0x7b054225cba9d98eu64, 0x4b9a4644c8c1e6cau64, 0x5f5dab707fc7477au64,
	0xa2650c332ea6fc4fu64, 0x596117c0085747bau64, 0xf0704670784724edu64, 0x53c9c56fe2e94379u64,
	0x4c1d5e71e14b10f0u64, 0x9ecc599323b16521u64, 0x578c5ac788ff9225u64, 0x4eee523c3dc95f76u64,
	0xd62526f8e8245124u64, 0x78761b06e4682974u64, 0x7ca9bd890e444134u64, 0x1dd2e6dd4cc41305u64,
	0x21873db0e031bf7cu64, 0xafa3a4cca3e3e8adu64, 0x71c2282d5df16e4du64, 0xf057459586cdeb8cu64,
	0x4f79a3b5f808a652u64, 0x51b58d7c5a5d85dbu64, 0x263fc9ffe98bc388u64, 0x68cba7e7bffd7d06u64,
	0x3f8c984f953728c1u64, 0x1dfe05758e716983u64, 0x49ec0fdc97128476u64, 0x1752f9fc30c909eau64,
	0x86f34ffc8a8c3c98u64, 0x14d36176d6df20f4u64, 0xc90e45c5ec58c801u64, 0xfb6435bb262363b1u64,
	0xa4e6a681cf54a942u64, 0x6f5c8de1616d1fa8u64, 0xf0937bda27277c5bu64, 0x191dd9049d45f398u64,
	0xdcaf9064ed42e39du64, 0x1254fa28500d2ac4u64, 0xf8ea0b52dd2fbbb3u64, 0x5ecba59731174e03u64,
	0x66b7fe6ee2939c99u64, 0xe320e2b7bdd0a576u64, 0xdf2b8125622b9e2au64, 0x5b13fa01203dfc61u64,
	0xc02950a28987230cu64, 0x47541d397cca32d9u64, 0xecdde5ca75fd1859u64, 0x5db52acea21e5587u64,
	0xec0081ee8cdbab48u64, 0x4a44f91ae4fca277u64, 0xeaa99120dc447512u64, 0x98b522685ea42db5u64,
	0x1bd2bcc611a28da6u64, 0x4a5b90a509cba427u64, 0xfd9d90a27c84ada8u64, 0xf46391d4467fc1acu64,
	0x85186c2f86d2dbd0u64, 0x8e81bf6fef2606e2u64, 0xc2fb30f85aabc069u64, 0x0a03a3c3ad2c4354u64,
	0x6fd052a4b92e1e07u64, 0x0f61a0e169f0d9bau64, 0x8b3f31e062bdfe62u64, 0x24b0ab1ac04810d8u64,
	0x5fd4c183dad5a310u64, 0xd6228b34c0288b2du64, 0xe384b72d9b2e48b4u64, 0xf49a49ae9a3fc516u64,
	0xa440b1c9cafe0cb6u64, 0xbb7fa02ec682f66cu64, 0x898f1cbe177a411du64, 0x41699946c98e7c13u64,
	0x19edfeb465cf92d9u64, 0x77a97f2c8a83bcb0u64, 0x982faab2298f5654u64, 0xc2f1bd86990c1194u64,
	0x1ce17fd6e56ffd05u64, 0x51c2cabc9c358b9du64, 0x07b70d2b8129daacu64, 0xd369d7aab22e6bfeu64,
	0xda66aab2f3226443u64, 0x8d816199cb5971bbu64, 0xf0d56ce2021b2118u64, 0x4c4f866cb372a69du64,
	0xec8ed21f3e20b3b6u64, 0x74557cd959772cfdu64, 0xb5604b4c2f792b2au64, 0xcbaca2d0e25d248eu64,
	0x0d72a67696e7ce0du64, 0x56f355d9a52ddc40u64, 0x10918bb594d7260du64, 0xe79be59e1c6850d2u64,
	0x2dc8da657c20020du64, 0x1bdfbb28a1735843u64, 0xdb3c5b0e4e38a0d0u64, 0xe5c8da62dbd72b71u64,
	0xe1b50fd75ad0628fu64, 0xe4387eb709964157u64, 0xf5e631f7ba745768u64, 0x8143b8f96046aa74u64,
	0x91bd3e5fae641abfu64, 0x59709188f37af0b5u64, 0x40b55aadb541deebu64, 0x0f54388ff13c141bu64,
	0xd4c1d1a98da0695eu64, 0x80c359ef7f100f59u64, 0x506977c5eaae6fd8u64, 0x4b3577825ca959d0u64,
	0x58d459abca8e37e6u64, 0xd9246655d94e807cu64, 0xa768c63f09664a79u64, 0x7ffaf09bb052290au64,
	0x318acefd90ae1182u64, 0x2ca59ef8faa5fe5bu64, 0x8c9ce9c5a8665affu64, 0x71654d7f6c8fa7fdu64,
	0xa16841be9190083cu64, 0x07942aa92d981f8du64, 0x7acd5d4b2e6fa5cau64, 0x733f241ce1eda0fbu64,
	0xce1e7834d0478e61u64, 0xfbb36018446f6ffbu64, 0x06833e9341e44057u64, 0xba4d0fbb3c565020u64,
	0x1f276e52d1db2575u64, 0x3a53b09f35143876u64, 0xf34cce64e92d5fdau64, 0xa779ed5acb51847fu64,
	0x6831f101eb7404d5u64, 0xe3e14d3ade63d182u64, 0x664f799ecf8149d5u64, 0x7f16600301153c21u64,
	0x31389f8c544a1494u64, 0x13aebdd5bc9cea06u64, 0xdf3df8859f7cf60cu64, 0x80c08a7e655958b7u64,
	0x48486659f5680b7eu64, 0xae3984e8a66dbfadu64, 0xcdd81c7b692d39b6u64, 0xc359beba9cba845du64,
	0x7b4885a61d9ddb8bu64, 0x5345d9d6a00a1711u64, 0x3df0c05271e9256fu64, 0xeae0ee8bd17f4447u64,
	0x00a2689cd7bc7c01u64, 0x17061db79ab93ebcu64, 0x31ec6584b0ee49f7u64, 0x626831ad8039bbb4u64,
	0xc6da110d8162fc8bu64, 0xbe3383fb2e988462u64, 0x358e89f4a0a8cf33u64, 0x32d2a34af6bbd749u64,
	0x6b2ea16bd8859b6fu64, 0xabd4b6541a936dc4u64, 0x1b97cbd6f46ccb9au64, 0xaed99fe2765e36d0u64,
	0x719deb3c8a753615u64, 0xd34672a60ae995fcu64, 0xb3ebbd9a12f95e3du64, 0x3b45599c6b8abeddu64,
	0xbc0ec9423d8f067bu64, 0x9279912c716ca9fbu64, 0x644cb595426150a3u64, 0x7b99d969dbe85d6du64,
	0x2dcea86bf4ea3c87u64, 0x32d787c3529b5ed4u64, 0x167d5309122b0957u64, 0xd996c0f64a481fceu64,
	0x0ec43720e7ff8fc9u64, 0x87df511b7c188bf8u64, 0xa27ef8b1a3897ddfu64, 0xff60a2d6f5a419c8u64,
	0xe03526ee6d8f0588u64, 0x004c23b8aaeb88beu64, 0x7c44ee6af162cc82u64, 0x00c023a9064bac0du64,
	0x2e91737d9ec852d6u64, 0xef63d90e138da0cbu64, 0x0fe12bc9e645df46u64, 0xc4887070644d8d36u64,
	0xd782fc5f61d03f3au64, 0x219dd6d7d4bcae9du64, 0x2ebd9c611d46ef0au64, 0xa287aa0cae28b107u64,
	0x00c48918c071d4e3u64, 0xebdeae020cf3d8c9u64, 0x17a51fe4cbeea648u64, 0xbc1aaa2883f8ce49u64,
	0x316d4da21ea5b520u64, 0xe412527ab0517ddeu64, 0x87642677c39e2a69u64, 0x428e32219d044ffbu64,
	0x4e70e7496ce920e4u64, 0xbd52bd88f4bed7a2u64, 0xfc252256ed5a1826u64, 0xbf0ce2048aa99c1bu64,
	0x5c2405727af2a1cdu64, 0xca1f7dcab298a34au64, 0xdc740b991272c72cu64, 0x661e4a84ecf38f0au64,
	0x39fb8e8a50164985u64, 0xca15345fb0a8fb92u64, 0xea58086d25e31fd5u64, 0xdd0423f095ef2b43u64,
	0xea026270d55dbffau64, 0xd33059e8bbc44382u64, 0x9f0f9be2db38ad9fu64, 0x5d65e0996ab165a1u64,
	0x25ecb64958372dc9u64, 0x5d3a8c56ff24478eu64, 0xf610999318cae412u64, 0x001eb72b63bdb74eu64,
	0xa8d3ecc33c6b99fau64, 0x09fcfb7f7d47ec2au64, 0x2f716b94412e21a5u64, 0x15b686230b27e9cfu64,
	0xdb4864c0bb3b379du64, 0x45bd96ca425f3922u64, 0x83e7677eb0060d98u64, 0xa23fb8ff99842ed0u64,
	0xba4db85a7d67f014u64, 0xc6c47829550e8adau64, 0x4cd313a6a47faf8au64, 0x11a417061d428454u64,
	0xbee08ca7ce7cf4e4u64, 0xba7f6d2d65135612u64, 0x47aa715a615c3883u64, 0x4559cfeb5edf5541u64,
	0x932d994715257ed2u64, 0x58d082031363adc5u64, 0xb2fc6ddd5ead9986u64, 0x2b7d86a49d86d228u64,
	0x4f1fa3e5c081070eu64, 0x93a603a01b97d706u64, 0xbcda84f4204b3dedu64, 0x68894d74614e422du64,
	0x8f8bcdbcced698b1u64, 0x96b5781b38ab08c6u64, 0xa0b9b00118797522u64, 0xda00443adbe6612du64,
	0xf07a3bc381a8e34cu64, 0xb9625fc84d340e93u64, 0xce9aa2ae679c9ebeu64, 0xaab50c21540f1b27u64,
	0x9562f2920ae9f250u64, 0xe4e9bca9bfdec46eu64, 0x70317f7bf7aa309cu64, 0xc5a848acc773dd83u64,
	0x28c0434fb1a6a683u64, 0x7873d626ba4eabdbu64, 0xfd7c2616ec18edcfu64, 0x8b41cf60f1538a4fu64,
	0xae7fb7057fe0fe79u64, 0x6dd2280dba5ee0d3u64, 0x9233597afb4c224fu64, 0x411f52ce62e087f7u64,
	0x7eeb4b08853ff762u64, 0x5c9770fb147a7365u64, 0x94aa0ebff6b8f547u64, 0x6826b97bca89f322u64,
	0xae0f9f2413c0a2f8u64, 0x29dd6a4ec840caccu64, 0x0dfacdc57f845746u64, 0x9c8f8b60735ac92bu64,
	0x75254d1fed9d6239u64, 0x3f14c7b2579135edu64, 0xf856504dbb1a657cu64, 0xec17da67f99a112bu64,
	0xe541f47cb948b097u64, 0xf37278cc5525a967u64, 0x0f9d52c12af77fb1u64, 0x159fa33bd68d89d7u64,
	0x5c6502c55c48586eu64, 0xb6864b3cdbc88973u64, 0x9acd0ec907afadd5u64, 0x824c76cdcecba9c3u64,
	0xdeae54a53d5c0944u64, 0x0660d9233894495cu64, 0x3edffee31a6ff304u64, 0xae6050befff942ffu64,
	0xf86b5641907322c2u64, 0xe7dae768a8d7c6ccu64, 0xd31138f2728eafc4u64, 0xfb615ab02b88e149u64,
	0xafb0e4841b4c33d0u64, 0xb257422112f94f00u64, 0x2dff94f8667f90e6u64, 0x55a79d3f39cd18d8u64,
	0x5a736ee3dc47669fu64, 0xafb086cee3238f5du64, 0x050b5a68403d48a0u64, 0x5640526d0a6b3c11u64,
	0xc531f0a23f8d369cu64, 0x9fbf2e0b809cbf43u64, 0x2d1ad5926c720ce1u64, 0x6d92973305b06235u64,
	0xb3909bb30ea5b961u64, 0x4e172150350dcceeu64, 0x066201e17ab70826u64, 0x4eb22f6263f6922fu64,
	0x05aa68036bbc0470u64, 0x60ef56c7326e8658u64, 0x2439ea78c0ccf875u64, 0x1679d054aadd9fb4u64,
	0x0015b7bbd4e17649u64, 0x930dc11581b50f8du64, 0xe580d20db1026a80u64, 0xa9195c0437ffd616u64,
	0x34b19e06cf8b2505u64, 0x0e86788d29dad927u64, 0xdbb4e8a8857a455cu64, 0x11a7c03ff35f858eu64,
	0x2010ea6eb4912f6bu64, 0xae19e8dc33afc0bau64, 0xeca53b6d7d2373e2u64, 0x7d43006cf118d379u64,
	0x2ccb81b6cc5ce500u64, 0xa518737309d86dabu64, 0x5b43df34466c6b9au64, 0x7a9cfebe6fc74b17u64,
	0xcc4446e0b71a823du64, 0x3edeae74d91eebc5u64, 0xd9fb8110f8694292u64, 0x91b036ad2c33caa3u64,
	0x9109e68a25c9b0c1u64, 0xf8a67cfcb9cd0a6eu64, 0x84a73cbce230a1d9u64, 0xeff65ea87e48ce7du64,
	0x9ad9addfabe1b08au64, 0xb2ee357c1ec0179au64, 0xd02b3265dee64202u64, 0x96b7971f07c0347eu64,
	0x11551a4baf8bb082u64, 0x2e48b6e112d3497au64, 0x67fcfe9f23310af9u64, 0x68b80723aa626868u64,
	0xbdd6931701bd027cu64, 0x2dd541256e65113cu64, 0xa7174c3d1dc7a114u64, 0xb7caac2195a7a622u64,
	0xad010a52591f03d9u64, 0x492570fd8364c25bu64, 0x35a988d189a40369u64, 0x2d1c49e2fb6f2350u64,
	0x69a930b2abf15211u64, 0x6590b16bd12ff633u64, 0x7b09ad9b1274da47u64, 0xe0b002f3a6861999u64,
	0x5ca122115da92535u64, 0x987779077510e8dbu64, 0x5089ae703bfb7217u64, 0x75d1e0790b9e60b2u64,
	0x6c17483b4c21337bu64, 0x6b705013f35e640cu64, 0xe027075deebcd1acu64, 0x4e41ea16d97baf2eu64,
	0xbaba0dd8ac5a1fc9u64, 0xc2594722b6bbde56u64, 0xaf63e777e72a3e19u64, 0x66077676cbaa9916u64,
	0xa53d5d27cf627671u64, 0xa4cb570f792399e7u64, 0xaf0353c513dd4e79u64, 0x8b59fa80cb20ee88u64,
	0x591ade5ca8a73d0fu64, 0x1eabc67797341ed2u64, 0xeab570e6ad930bb4u64, 0x6c722f6f656ef7e5u64,
	0x62f2845c3528358fu64, 0x5d0eee5c743800c6u64, 0x31e35097f72eac38u64, 0x9366538a7823965fu64,
	0x4946cce6676d73bbu64, 0xc13ecda1eb3353cdu64, 0x49c262ac04c4625fu64, 0x8ead783027e9cbf2u64,
	0x3ec2f800eaf8d363u64, 0xbf3021015eda9021u64, 0x128d569d290f7187u64, 0x94b4f7ee447d4f5du64,
	0x2332159053cd9e08u64, 0xc5954c233588cddcu64, 0x7a31c83ccbb92dc6u64, 0x55f7becaa13395bdu64,
	0x57ab6df03fac711bu64, 0xf08590614c6ff374u64, 0xda5998e17a53b268u64, 0x5fe308e6e6dc6f92u64,
	0x89adc19f2dc53773u64, 0xd083da63939f245bu64, 0x45227cca7a9d2d06u64, 0xad994a04b48a9b6cu64,
	0xab9ece082967b870u64, 0xfc93f18fb0beaddeu64, 0x287fe100753ac213u64, 0xb5d4594c6f718c3fu64,
	0x7374f98e99a3e691u64, 0xbf50aea45d44c7c5u64, 0x1f24689ec400357au64, 0x3b62e83ab3d15bfau64,
	0x7fceb4e49424dcadu64, 0x7f54a2805ad27d56u64, 0x7fcfa2d7ba6d7904u64, 0x5049e4518245a357u64,
	0x07b075d0aac41f50u64, 0x1936073479af2084u64, 0xa4325aad3ba96109u64, 0xb1b0a87decb20c4fu64,
	0xa5f731128e7757aau64, 0x4db24a1fdfd4c1ffu64, 0x02b35e5f628c8effu64, 0x6e8fe805b56dfdd9u64,
	0x631a5baafa316423u64, 0xc7cf050f2b5dc602u64, 0x7e005b57b1c3709cu64, 0x3cea7f395ec7d4dcu64,
	0xa6506a794debc6f9u64, 0xc1c3771cfa955196u64, 0x2bb8efb672830378u64, 0x256717c30da67f2cu64,
	0x16b36229b783a098u64, 0x4ced28cf3e735396u64, 0x88900ec29059761cu64, 0x728e1af9dc380206u64,
	0xeae45c1da57ab401u64, 0x87a77fc2e3d7ebd7u64, 0x8af59908f2f1aa39u64, 0x4ba0b1ee234e8e9au64,
	0x4608ca5b37afee89u64, 0xe3ad6872fe087cceu64, 0x7844e6326e36b828u64, 0x3ab10e8034ce0456u64,
	0x9cf5a7e624b0774bu64, 0xa91068ccf81ca01fu64, 0xf67f6545cb5adba0u64, 0xc931ae22d5a83bb0u64,
	0x9e1bc2893c7d5590u64, 0x74b639092dda2ca3u64, 0xa8223d6711312a7au64, 0x346948b598bf95aeu64,
	0xd3226190f16e703cu64, 0x66898ccc64baf1c1u64, 0x6695a0414693ca9au64, 0xb87ee26fd1514ff0u64,
	0x59665d1f7c542fd3u64, 0x30a3a40ad0acfd04u64, 0x955bd1dff7623d00u64, 0x554ea5e991201b1fu64,
	0x1522a95590a01045u64, 0xc4975c893df2ed6au64, 0xdb71615e3e702f4au64, 0xaeff288ea60c0188u64,
	0xd353a2b8e8f8e39eu64, 0x9e114ebc1db98d8au64, 0x45f2990e0e793559u64, 0xd33341d365733912u64,
	0xd94ce74cd1778bc7u64, 0x86c8d3107e80031cu64, 0x17a9c1adb1192d16u64, 0x062be3de61db4c9eu64,
	0x0193e55bf27b821eu64, 0x425616ccd850d573u64, 0x8bdcfef42e89644fu64, 0x889fe19c5cdd0cd0u64,
	0xb6c58349505a069cu64, 0x88527f2c3c758c85u64, 0x6b6cf9286dd29f59u64, 0x9ea76bd9f1934602u64,
	0xaf0268d258b617e8u64, 0xe277ed1c3707eaa6u64, 0x1b03240ae336b6fbu64, 0xed5daf4e0c95df54u64,
	0x42fa8c6b25d8b2bdu64, 0xb81bdec9d8f3f1f8u64, 0xbc81a1b675ae4bacu64, 0x81b2a3faa26ad0dcu64,
	0x38e9ce65ae3cf007u64, 0x284e170985bea4a1u64, 0x1cb2f5154f98437au64, 0x1e173ae30bdf9bfbu64,
	0x1f3f32cdfae01485u64, 0x5fa4ea3a19501dc7u64, 0xa318c69258ed2395u64, 0x57be48d66ffca2dbu64,
	0xc80e2df139529e15u64, 0xe9ddca96a32502cdu64, 0x69ca89427d93288cu64, 0x5cfe9b16e26bca3du64,
	0xc8a61e4d4498130du64, 0x8d8ff0a28d53408du64, 0xe7135e44b365a301u64, 0xf47376721ea6b0ccu64,
	0x204e1ed989e6916eu64, 0xe5193f2c03c32fafu64, 0x03b0a7fdab48558cu64, 0xd531a52ea225ce57u64,
	0xb337230bda964bb1u64, 0x24fb471a041d13bfu64, 0x4bfc897f70461504u64, 0x29a153d0b5d0b674u64,
	0x9d621e740aaa14bau64, 0x5e892ad283faf8f0u64, 0x52d12dec54530e12u64, 0x5a1e39e4de2a20cdu64,
	0xfcb1be0335f40df7u64, 0x2d89b4e72c12fe66u64, 0xc65586ce0a6e3f40u64, 0xddf92f6157316b5eu64,
	0xe876f5e18d65c89eu64, 0x6b35a8fad154c6ceu64, 0xafe748ad3cc075c7u64, 0xcfb0ba6d35436430u64,
	0x7d0d2ef92daab9d0u64, 0x64a3c4b06c971b9du64, 0xdb71310f021951dau64, 0x75e0278d15e37aa5u64,
	0xd0d18702b846bfa7u64, 0xe7592ad26017cbf2u64, 0x8129e0359778ba7cu64, 0xe8c5d0516dada995u64,
	0xa6047c3c89c6459au64, 0xb073c25471b688f9u64, 0xd9f9efbe72427752u64, 0x955cf557db6bb30eu64,
	0x316df3fab0194a6au64, 0xe9c8a9be128db0d3u64, 0x02027e5e5bfd10aeu64, 0xe3444c3b1c80d467u64,
	0xe3c624b1914bd0a4u64, 0x78c2ab396033261au64, 0x5db9a3b9ddd5daf1u64, 0xed64a9d10d64d395u64,
	0x67cab037e7c594e6u64, 0x9ae5a71bd2679b1au64, 0x07bf96017f7c9629u64, 0x472cdb75b55fc7f6u64,
	0x53c532e458fded7bu64, 0x9d23162c36b0d761u64, 0xd575555da7712b24u64, 0x1829807893415606u64,
	0xf419569f35bdac03u64, 0xbdeaf19bb0910575u64, 0x0d30fddb1fcc0be6u64, 0xe264f441412e67a2u64,
	0x3b5dde40003d11bau64, 0x1c0a3f1b960eecbau64, 0xa92c1f9012301f49u64, 0xa4b4e184591a0b99u64,
	0xf3e25dc31e9bb1e1u64, 0x1cf79ba85889bbdcu64, 0xb549c85571c7e927u64, 0x15a6234241dd3d66u64,
	0x9bbe6161d5bf7fb9u64, 0x3e94a904c1d5660bu64, 0x1f817d27df5ede4du64, 0x398be018bd310fb5u64,
	0x96126c1e1ff9bd91u64, 0x9cbbfaf8560739e7u64, 0x50b0711f5af445dbu64, 0x80c0b90b26fc0ecdu64,
	0xb3dd43871eaa7a2au64, 0x2748da2053d2cf68u64, 0x5d9d0492c0aff08eu64, 0x30ba84d55d6b4e80u64,
	0x045544d16d6c7ee9u64, 0xbe326759886c1df2u64, 0x3ff48346cb9585a9u64, 0xa242b5caef7faeb4u64,
	0x5725c15b63c0e4dfu64, 0x791f19ca44b6936du64, 0x66e1144bbfb2a0adu64, 0x01d6c693df18d4b9u64,
	0xbd85e6896b992241u64, 0xe049a665b648076bu64, 0xe4370d6c8be7da12u64, 0xd05ac7121bb3591du64,
	0x5cc71d44df54a21fu64, 0xc0ff6f30f24ba260u64, 0xee952bc27e4d6f6au64, 0x39131e8ace5f78afu64,
	0xbfc4facc0177363fu64, 0x75e5f03d1ecfdaacu64, 0xfd8d39ff4e7d20c3u64, 0x8ba3c8802c185d88u64,
	0x4c5a24a76d85bec3u64, 0xd22af8352c710c8du64, 0xc2574da33566d41fu64, 0x2c654a60d8ba6f5du64,
	0x42c0bce712e11c80u64, 0x450f02908b825f15u64, 0x602ebe5d30245e20u64, 0x4bdf56061d1a4124u64,
	0x8533e1aed8d4f188u64, 0xad8007b6bb6831b0u64, 0x8e53dfdb4b9559e6u64, 0xbb888efc56f185d8u64,
	0x9517eeffe0fcfd4du64, 0x1b775c5ad6fb8bf6u64, 0xca1bf7937b358b4fu64, 0xe90bdf28dbcd3152u64,
	0x4cdbdb7a28991271u64, 0x97ab25ed2f7e1932u64, 0xaca0ca9671f594cdu64, 0x9f1911165e3617f4u64,
	0x1d4b4bb19eefa27bu64, 0x194c8bad22688563u64, 0x363ef709b44c3f16u64, 0x71b3f89fc2e3df87u64,
	0xa5fdf44c7f542b46u64, 0xc5c46233c02972beu64, 0xe9f12cebb017b9e7u64, 0xf30f3509ca176635u64,
	0x6ccfafebfcba2642u64, 0x7ec427639de49212u64, 0x90f32e25adb209fbu64, 0x6ac8ab454927df1eu64,
	0x0c88f69e01dd9f8eu64, 0xba35894826c86501u64, 0x856866449182fbfau64, 0x9516474aaa4ba2fcu64,
	0x4749762c1f0c6fb7u64, 0x71bb4a4cd6f286eeu64, 0xaf5b9a68bfd0a35au64, 0xfec163448b4f5fbfu64,
	0x31064cf3405648f0u64, 0x3969f4f0ed25c926u64, 0xe502b6f5f1b9e3d1u64, 0xa38ebddd32f4e5c8u64,
	0x2ea3f41e1b7433c2u64, 0x27e7a8ea7d4d4371u64, 0xaa0256d0cdf67ea0u64, 0xad2941a63120f35eu64,
	0x8e21979248bb5a78u64, 0x3b94d518bf63250eu64, 0x9b7a255f05a2b57du64, 0x51dcbe32463d36d0u64,
	0x7134705b97df4d9au64, 0x07d27ec803701b34u64, 0x79ec69bcd26b3cd0u64, 0x6090672aa0a2da73u64,
	0x4a8312ae2ff6125au64, 0x331b42ac0972ed7fu64, 0x199963475c0e2a17u64, 0xecf69b8970ad5539u64,
	0x61f4ca937dde5886u64, 0x1f30469f274e1b90u64, 0x35e00938e61104eeu64, 0xf5c8a9a4787e2d0bu64,
	0x425a9a3603c77379u64, 0xbe90b29c70ca9752u64, 0x7fd491875b1b343fu64, 0x3bb6f583a6e88348u64,
	0x97969c6c3ac362bau64, 0xbfff9f4aa0f79415u64, 0x96545ec61c9bd155u64, 0x414045d6393ce369u64,
	0xfe83a3593901d798u64, 0xbcd7192ba1d45fbcu64, 0xf787e0b47617474cu64, 0x1b1b7a29ac69eeb1u64,
	0xf0b8c110260a3c7eu64, 0x3e635cd1e1a2f8e4u64, 0xbf72847084e604b5u64, 0xd81dc002210e662cu64,
	0x51a1f99677117e5du64, 0xe65b5deaab205771u64, 0x4fbdc2c95b8995a1u64, 0xbd88d5f517fac94cu64,
	0xb1e6e68e076d0abfu64, 0x8018a6f03792b808u64, 0x3704c28db17f4695u64, 0x1aa18333d1589b4bu64,
	0x5cbbbbf1d547a311u64, 0x6acc6fe57795cd7cu64, 0x5a27fd14539fc1c9u64, 0x6d0e9dfe17d7b2f1u64,
	0x2ad1640def3b0f46u64, 0xe2e282c92d418e6bu64, 0xfb992d766616dd23u64, 0xcda85d80da427e8eu64,
	0xef8d8502abee2278u64, 0xd28fe89204ef3369u64, 0x6834028c447f8fc3u64, 0x1dbbca5ad9723834u64,
	0x5441fa5c1f8c44e9u64, 0xd3a835d6f73befddu64, 0xde38c8e324ac7e53u64, 0x6cf1e70a13851309u64,
	0xd5a4016877dea292u64, 0xfbf7be7394fe09f8u64, 0xffe162e780ed1c33u64, 0x78ce030222787822u64,
	0xeb768d2af3f4caadu64, 0xd1931723603511dbu64, 0x3828724707339818u64, 0x308b9dc9c97cc652u64,
	0xf76fd39c2d653f6eu64, 0xd5255b704c0c0b75u64, 0xf415899f54946643u64, 0xc2c0c674e0fc73c7u64,
	0x1d817d1bc1f7cdbau64, 0x2c9bc1d38df9a358u64, 0x6b0b908839a02efau64, 0x9c8aae8fc42eaab3u64,
	0xd6d4de5c847c81a3u64, 0x8d7da44657018463u64, 0x34b0aaaa512d899cu64, 0x87d5962bfa2444e3u64,
	0x85d3d5a3a5cd1be0u64, 0xcdab8e6f45e07b67u64, 0x22e3f05564002663u64, 0x104d11d8217bcb14u64,
	0x2f656b63a5dc894eu64, 0x78d7b8f69861e105u64, 0xaf07633a57ee27f5u64, 0x750661b6725a31f3u64,
	0xe347ca82b590d876u64, 0x6310610e9def4a4au64, 0xcd3b0ae78a833564u64, 0xa38133021633b8ccu64,
	0x7c6ddfaa3844f716u64, 0x68c4566c1b72b7b8u64, 0x2417661935b66dbbu64, 0xb2345f5df9eee447u64,
	0x373aef651c65fa16u64, 0xafc28337c938628fu64, 0x685a42a06e458a55u64, 0x0f2c2e1e166459c3u64,
	0x42c86d5b256a5fa8u64, 0x81d4445710a28c1fu64, 0xbb3277e5935982a1u64, 0x3cf10cd8471fee6bu64,
	0x6c8229d8f746f7edu64, 0xbede1479e7791167u64, 0x352a27383e5f4cffu64, 0x11c70f485ce7dbe3u64,
	0x8563a34a848ecb7du64, 0xb1305a5f8dd66ae5u64, 0x4aaa72b51f6d5abcu64, 0xa24f9d31bd359d29u64,
	0x9a1c586e2af996eeu64, 0xe0e6851a148d1d1eu64, 0x91eac662ed0704c4u64, 0x71d70f99bdb1d9abu64,
	0x3dfde387844d0140u64, 0x84f3ac027e2e1ad1u64, 0x771636d573f537a2u64, 0xbb0f6970ea6a0022u64,
	0xd6063031524f3683u64, 0x6b450e760c14b4f9u64, 0x4766874137588552u64, 0x6f6db2b52fe74807u64,
	0x4e039b6241dbd5eau64, 0x7fca257462966b6fu64, 0x717f20505b4583b4u64, 0xf9afab246367b2ffu64,
	0xebf46540e121a046u64, 0x73cec0c86109c2c2u64, 0x7f8ab35174170359u64, 0x67817dd6fd4bcbe6u64,
	0xe60a4a2c7ce608b5u64, 0x86374d46c7952a22u64, 0xbdaa8cfcd9abaedbu64, 0xcfc3798147678e3fu64,
	0x1cf5f4a72679ef1du64, 0xa2c5b7028bec6d6cu64, 0xcad202a33eaf8efau64, 0x6f463aee03ae3b6cu64,
	0x1f2a450668391a72u64, 0xa121d3025427423cu64, 0x8e147641fa49165au64, 0x97a9a97b1942b94cu64,
	0x490e0a5784df8a7cu64, 0x8bca18578c1c7febu64, 0xd72c70cc4d49de47u64, 0xc8b3995d0e3a1149u64,
	0x3bb76e8387b795d6u64, 0xf8fcbf1543eff0d4u64, 0x14c32d764bde48edu64, 0xbc0e24e9e706ac81u64,
	0x8aee9af45168dcdeu64, 0x3166027ae640dcc3u64, 0x5565fa46bc01b33du64, 0xb6112879bc42e75eu64,
	0x1ad167470d516d90u64, 0x3ab233270a0de407u64, 0x48a6c74f63160deau64, 0x90b66d3bf50c525fu64,
	0x8a3b24ed31d8d8deu64, 0x03e8024a7395fdffu64, 0xbed17c33f5b21530u64, 0xb732ff02ce49db8du64,
	0xad2b5020cb3c4720u64, 0x8eb04ba3beb2df96u64, 0xc711b531c2e02baau64, 0xa3cd2cb272a33d80u64,
	0x95dd892e6d926719u64, 0xeb594261c7dfafbau64, 0x796fd35f6ca3c64eu64, 0xbfce50b9eaa4031cu64,
	0xfaf66b4d75274146u64, 0xff29ac77b0e5cc05u64, 0xfadb3c50b80f16e5u64, 0x43bb58b4d375549bu64,
	0x4c0c471370c2a2f2u64, 0xf43431bfd3aac80fu64, 0xe53c866b7383ec99u64, 0xffd710b7c403c9f9u64,
	0xdd148077d4c1fd5cu64, 0xa6ee30b9f138cabau64, 0x708d288f29c1370du64, 0x397bac4791f932aeu64,
	0x28979a7107b5e3e4u64, 0x39266d5cc3c97e7du64, 0x5f84cc5daf00047au64, 0x74f6ca0a12da5243u64,
	0x82ab3585a748a77fu64, 0x7149bf0d8470a69fu64, 0xdb3d6c6d6a7fd121u64, 0xa367205aef0fadadu64,
	0x7775ac70072c9738u64, 0xc638adc5f261590du64, 0x05396cf28c5fba32u64, 0xed03ffb2e5b97059u64,
	0x9067f5bdbe351db0u64, 0x10a9b9fd79fa6b58u64, 0x54d5bd3395827b1au64, 0x13712905022adeaau64,
	0x4c2846f2eaa8d9cfu64, 0x4b07f177bc18a10cu64, 0x2bd40d3413edf8e6u64, 0x0d386ab445518fa8u64,
	0x3d91a21c5e088d52u64, 0x19b18759cdf8fcfdu64, 0x01fdd988966ef3bau64, 0x368b48b5d58bfa69u64,
	0x3359b2917223a314u64, 0x30f97440639a60e9u64, 0xd12daf9177627c9fu64, 0xb52a768a57f8300bu64,
	0x151457c399d36a4au64, 0x2ee89da379f99c12u64, 0x9fb88bac59480f16u64, 0xb421d7c71cb582a1u64,
	0x3d42cfdb051c63b7u64, 0x5094e223efc89214u64, 0xd032dad07bc5396cu64, 0xf05dbb14ff777495u64,
	0xc880a0ea9a8b932eu64, 0x96c30e722f9e41efu64, 0xef697e4a413d6ea2u64, 0x81241aabc07127d9u64,
	0xa64784211442885fu64, 0xad6dfebba2790a16u64, 0x5b386d261980dff7u64, 0x1aa36f461e34f38bu64,
	0xbb12a85cf4cda3fbu64, 0xe59b134cff1d41adu64, 0x5f3abf7ec4566ea6u64, 0x56e4817d04ea843bu64,
	0xe01cf339094e2802u64, 0x5efa7dab8513e642u64, 0x7e0d3dbb55b8eee6u64, 0xe3b005c44cfc62d8u64,
	0x090702fcb2811433u64, 0x7e46e87f779dec5bu64, 0x56fc33e157eaf78bu64, 0x609c898f87684597u64,
	0x7ffc539986558611u64, 0xb637d30c712789b6u64, 0xbdb74dd3ec42bf7fu64, 0x1d38b12425ddcf86u64,
	0xa38eba1dca066f7eu64, 0x92aec701678c98b6u64, 0x6eafe8cc1b5e3579u64, 0x6c255325b7745ed4u64,
	0xa157e72eb6178d81u64, 0x49546d4ae3bc3bbcu64, 0x8c7bfa96b3294080u64, 0x446872fee94f54feu64,
	0x04a7a4181d7cddf2u64, 0x026a2215f97049b1u64, 0xa80a2374140d2d32u64, 0xe8be3c08c22adbf6u64,
	0x531c265c984217d6u64, 0x0a9b4f74fe6764c5u64, 0x48ef941f66711501u64, 0x95b1d810f1d028a4u64,
	0x27215abf9bfb6bdfu64, 0xd29d1598212ecd2eu64, 0x267582bed8aa46aau64, 0xc8ff3025b7ec03bbu64,
	0x53105847e28330e3u64, 0x45d8d0f0a78362afu64, 0x988f77b6da11f6e3u64, 0xa660757cee135096u64,
	0x7da856279576afefu64, 0xdc30fcd5447440dbu64, 0xc5e1cdd2b041eed7u64, 0x5fdba8abad3820d9u64,
	0xda517a125e33c1b2u64, 0xb7d117f523823ddeu64, 0xb0d8d316eef79d66u64, 0x1daf648cb5acb28fu64,
	0xf88c3f1bacd5c685u64, 0x91b3afd84a523d7fu64, 0x36cf06409a0fed1du64, 0x6c2823db22a77bdeu64,
	0xc1e253762c284388u64, 0x62dd1cc33243ff53u64, 0xad7156aa8cd0fb74u64, 0xdc6934b38938f532u64,
	0xbdd04da987de285bu64, 0x3ef325cefa015f8cu64, 0x370c2dbb13274416u64, 0xdc262b9c7b95689fu64,
	0xf245533ae4c4870du64, 0x6fe2d8a19d4c1ee7u64, 0xd59ccb9de51a6badu64, 0x994ce1d0bbbdf5d0u64,
	0xd33bd5af7bbce745u64, 0xd1e4973b06f48ebdu64, 0x266019f0ebd70251u64, 0x9cfd3f42178ddf7au64,
	0xe7027a9e2de26a6au64, 0x8fd52f08a9f07381u64, 0xfa094d52068812fcu64, 0x6c0150ca91c29370u64,
	0x51e30806202f09f4u64, 0xee2bbef3a888a32bu64, 0xc292aaee7b64c74cu64, 0xe9711f7438020d11u64,
	0x3b81082002e7a648u64, 0xb6154449e79797a8u64, 0x4662ff5aace91bcbu64, 0x8adc28490410d34du64,
	0x9fb51c41008297e1u64, 0x7758ce67f95c6323u64, 0x1aa7bc9d23226d9eu64, 0xae90fb1c24b39ebeu64,
	0xb46915d722a2a7b9u64, 0x08dc6dccd032b11cu64, 0x0a6ce5abad4c4bd9u64, 0x8ebc7758d7a37339u64,
	0xbe011d4567d09fe6u64, 0x782845f6d2ce4d33u64, 0x9ae6ff3503371ea1u64, 0x04f6dc6dbba1dea4u64,
];
